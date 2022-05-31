{ options, config, lib, pkgs, ... }: 

with lib;
;
let cfg = config.modules.desktop.xorg;
in {
  options.modules.desktop.xorg = {
    enable = _.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;

      layout = "us";
      xkbVariant = "intl";

      libinput.enable = true;

      displayManager = {
        lightdm.enable = true;
        defaultSession = "none+qtile";
      };

      windowManager.qtile.enable = true;
    };
  };
}