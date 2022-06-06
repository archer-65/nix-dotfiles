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

    services.xserver.displayManager = {
      lightdm.enable = true;
      defaultSession = "none+qtile";
    };

    services.xserver.windowManager = { qtile.enable = true; };

    services.xserver = {
      layout = "us";
      xkbVariant = "intl";
    };

    services.xserver.libinput.enable = true;
    environment.systemPackages = [ pkgs.libinput ];
    user.extraGroups = [ "video" ];
  };
}
