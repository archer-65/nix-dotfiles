{ options, config, lib, pkgs, ... }: 

with lib;
with lib.my;
let cfg = config.modules.hardware.audio;
in {
  options.modules.hardware.audio = {
    enable = mkBoolOpt false;
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

    environment.systemPackages = with pkgs; [
      easyeffects
    ];

    user.extraGroups = [ "audio" ];
  };
}