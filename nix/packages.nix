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

      ps = purs-nix.purs {
        dependencies = [
          "ursi.debug"
          "effect"
          "prelude"
        ];

        test-dependencies = [
          "test-unit"
        ];

        dir = ./..;
      };

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
          purs-nix
          mcpConfig
          ;
        ps-tools = inputs.ps-tools.legacyPackages.${system};
      };

      ciPackages = with pkgs; [ nodejs ];

      packages = with ps; {
        default = app { name = "hello"; };
        bundle = bundle { };
        output = output { };
        ci = pkgs.buildEnv {
          name = "ci";
          paths = config.ciPackages;
        };
        mcp-config = mcpConfig;
      };
    };
}
