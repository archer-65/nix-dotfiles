{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.hardware.audio;
in {
  options.system.modules.hardware.audio = {
    enable = mkEnableOption "audio with pipewire";
  };

  config = mkIf cfg.enable {
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    hardware.pulseaudio.enable = false;

    sound.mediaKeys.enable = false;

    security.rtkit.enable = true;

    environment.systemPackages = with pkgs; [
      easyeffects
      pamixer
    ];

    primaryUser.extraGroups = ["audio"];
  };
}
