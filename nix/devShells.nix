_: {
  perSystem =
    {
      config,
      pkgs,
      ps,
      toolPs,
      purs-nix,
      mcpConfig,
      ...
    }:
    let
      # Runs the purs-memo codemod tool (issue #7) against a local path —
      # `purs-memo src` transforms `src/` in place, matching what `nix
      # build` compiles. Run `git restore src/` afterwards to discard the
      # transformed copy: `src/` itself is never meant to carry it.
      purs-memo = pkgs.writeShellScriptBin "purs-memo" ''
        exec ${pkgs.nodejs}/bin/node --input-type=module \
          -e 'import { main } from "${toolPs.output { }}/PursMemo.Main/index.js"; main()' \
          -- purs-memo "$@"
      '';

      devPackages =
        config.ciPackages
        ++ config.pre-commit.settings.enabledPackages
        ++ [
          (ps.command { })
          purs-nix.purescript
          purs-memo
        ];
    in
    {
      devShells.default = pkgs.mkShell {
        buildInputs = devPackages;
        shellHook = ''
          ${config.pre-commit.shellHook}
          cat ${mcpConfig} > .mcp.json
          echo "Generated .mcp.json"
        '';
      };
    };
}
