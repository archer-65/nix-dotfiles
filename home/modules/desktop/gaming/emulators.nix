_:
{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.user-modules.desktop.gaming.emulators;
in {
  options.user-modules.desktop.gaming.emulators = {
    switch.enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = {
    home.packages = with pkgs; [ (mkIf cfg.switch.enable yuzu-mainline) ];
  };
}
