_:
{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.desktop.xorg;
in {
  options.modules.desktop.xorg = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    services.xserver.enable = true;

    services.xserver = {
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "ctrl:nocaps";
    };

    services.xserver.libinput.enable = true;

    services.xserver.windowManager = { qtile.enable = true; };
  };
}
