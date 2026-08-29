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

      ps = purs-nix.purs {
        dependencies = [
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

        test-dependencies = [
          "test-unit"
        ];

        dir = ./..;
      };

    in
    {
      _module.args = {
        inherit
          pkgs
          ps
          purs-nix
          ;
        ps-tools = inputs.ps-tools.legacyPackages.${system};
      };

      ciPackages = with pkgs; [ nodejs ];

      packages = with ps; {
        default = output { };
        ci = pkgs.buildEnv {
          name = "ci";
          paths = config.ciPackages;
        };
      };
    };
}
