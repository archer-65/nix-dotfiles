_:
{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.hardware.audio;
in {
  options.modules.hardware.audio = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    sound.mediaKeys.enable = true;

    security.rtkit.enable = true;

    environment.systemPackages = with pkgs; [ easyeffects pamixer ];

    user.extraGroups = [ "audio" ];
  };
}
