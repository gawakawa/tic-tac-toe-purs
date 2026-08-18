_: {
  perSystem =
    { ps-tools, ... }:
    {
      treefmt = {
        programs = {
          nixfmt = {
            enable = true;
            includes = [ "*.nix" ];
          };
          oxfmt = {
            enable = true;
            includes = [
              "*.json"
              "*.jsonc"
              "*.json5"
              "*.md"
              "*.mdx"
              "*.yaml"
              "*.yml"
            ];
          };
        };
        settings.formatter.purs-tidy = {
          command = ps-tools.for-0_15.purs-tidy;
          options = [ "format-in-place" ];
          includes = [ "*.purs" ];
        };
      };
    };
}
