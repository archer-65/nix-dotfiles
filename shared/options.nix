{
  options,
  lib,
  flake-self,
  ...
}:
with lib; {
  options.dotfiles = with types; {
    configDir = mkOption {
      type = path;
      default = "${flake-self}/config";
    };

    assetsDir = mkOption {
      type = path;
      default = "${flake-self}/assets";
    };
  };
}
