inputs:
{ options, lib, ... }:

with lib; {
  options.dotfiles = with types; {
    configDir = mkOption {
      type = path;
      default = "${inputs.self}/config";
    };

    assetsDir = mkOption {
      type = path;
      default = "${inputs.self}/assets";
    };
  };
}
