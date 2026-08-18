_: {
  perSystem =
    {
      config,
      pkgs,
      ps,
      purs-nix,
      mcpConfig,
      ...
    }:
    let
      devPackages =
        config.ciPackages
        ++ config.pre-commit.settings.enabledPackages
        ++ [
          (ps.command { })
          purs-nix.esbuild
          purs-nix.purescript
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
