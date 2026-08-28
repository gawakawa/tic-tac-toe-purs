{ inputs, flake-parts-lib, ... }:
{
  options.perSystem = flake-parts-lib.mkPerSystemOption (
    { lib, ... }:
    {
      options.ciPackages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ ];
        description = "Packages for CI environment";
      };
    }
  );

  config.perSystem =
    { config, system, ... }:
    let
      pkgs = import inputs.nixpkgs { inherit system; };

      purs-nix = inputs.purs-nix { inherit system; };

      node_modules =
        pkgs.importNpmLock.buildNodeModules {
          npmRoot = ./..;
          inherit (pkgs) nodejs;
        }
        + /node_modules;

      with-react =
        package: modules:
        pkgs.lib.recursiveUpdate package {
          purs-nix-info.foreign = pkgs.lib.genAttrs modules (_: {
            inherit node_modules;
          });
        };

      ps-deps = [
        "ursi.debug"
        "effect"
        "prelude"

        (with-react purs-nix.ps-pkgs.react-basic [
          "React.Basic"
          "React.Basic.StrictMode"
        ])

        (with-react purs-nix.ps-pkgs.react-basic-dom [
          "React.Basic.DOM"
          "React.Basic.DOM.Client"
          "React.Basic.DOM.Components.GlobalEvents"
          "React.Basic.DOM.Components.Ref"
          "React.Basic.DOM.Events"
          "React.Basic.DOM.Internal"
          "React.Basic.DOM.Server"
        ])

        (with-react purs-nix.ps-pkgs.react-basic-hooks [
          "React.Basic.Hooks"
          "React.Basic.Hooks.Aff"
          "React.Basic.Hooks.ErrorBoundary"
          "React.Basic.Hooks.Suspense"
        ])
      ];

      # `srcDir` must contain a `src/` subdirectory; `test/` always comes
      # from the real, untransformed working tree (the transform never
      # touches tests, see PursMemo.Main).
      mkAppPs =
        srcDir:
        purs-nix.purs {
          dependencies = ps-deps;
          test-dependencies = [ "test-unit" ];
          srcs = [ "${srcDir}/src" ];
          test = ./../test;
        };

      # The purs-memo codemod tool (issue #7): a separate PureScript program,
      # built with its own purs-nix instance since it needs the CST/codegen
      # toolchain, not react-basic. `node-fs`/`node-process` import Node
      # built-ins directly, so no `with-react`-style `node_modules` stamping
      # is needed.
      toolPs = purs-nix.purs {
        dependencies = [
          "language-cst-parser"
          "tidy-codegen"
          "dodo-printer"
          "node-fs"
          "node-process"
          "console"
          "effect"
          "prelude"
        ];

        test-dependencies = [
          "test-unit"
        ];

        dir = ./../tools/purs-memo;
      };

      # The one place that knows how to invoke the compiled tool (`purs
      # compile` output already ships `package.json` `{"type":"module"}`, so
      # no esbuild/bundling step is needed) — shared by `mkCodemodOutput`
      # below and by the devShell command in nix/devShells.nix, rather than
      # each hand-building the same `node -e` invocation.
      purs-memo-cli = pkgs.writeShellScriptBin "purs-memo" ''
        exec ${pkgs.nodejs}/bin/node --input-type=module \
          -e 'import { main } from "${toolPs.output { }}/PursMemo.Main/index.js"; main()' \
          -- purs-memo "$@"
      '';

      # Run the tool against `src/` as a plain runCommand.
      mkCodemodOutput =
        pruneFlag:
        pkgs.runCommand "src-transformed${pruneFlag}" { } ''
          mkdir -p $out && cp -r ${./../src} $out/src && chmod -R u+w $out
          ${purs-memo-cli}/bin/purs-memo ${pruneFlag} $out/src
        '';

      codemodOutput = mkCodemodOutput "";
      unprunedCodemodOutput = mkCodemodOutput "--no-prune";

      # The A/B/C measurement arms (plan §Verification): A = baseline
      # (untransformed), B = unpruned (issue #7 as originally written,
      # amended for soundness), C = pruned (this repo's shipped default).
      ps = mkAppPs codemodOutput;
      psUntransformed = mkAppPs ./..;
      psUnpruned = mkAppPs unprunedCodemodOutput;

      mcpConfig =
        inputs.mcp-servers-nix.lib.mkConfig
          (import inputs.mcp-servers-nix.inputs.nixpkgs { inherit system; })
          {
            settings.servers = {
              pursuit-mcp = {
                command = "nix";
                args = [
                  "run"
                  "github:gawakawa/pursuit-mcp"
                  "--"
                ];
              };
            };
          };

    in
    {
      _module.args = {
        inherit
          pkgs
          ps
          toolPs
          purs-nix
          mcpConfig
          purs-memo-cli
          ;
        ps-tools = inputs.ps-tools.legacyPackages.${system};
      };

      ciPackages = with pkgs; [ nodejs ];

      packages = {
        default = ps.output { };
        untransformed = psUntransformed.output { };
        unpruned = psUnpruned.output { };
        ci = pkgs.buildEnv {
          name = "ci";
          paths = config.ciPackages;
        };
        mcp-config = mcpConfig;
        purs-memo = toolPs.output { };
      };
    };
}
