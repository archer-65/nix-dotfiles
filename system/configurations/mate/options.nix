# Mate, main laptop
{ config, lib, ... }: {
  modules = {
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      monitoring.enable = true;
    };

    services = {
      ssh.enable = true;
      gpg.enable = true;
      flatpak.enable = true;
    };

    desktop.xorg.enable = true;

    core = {
      boot.splashBoot.enable = true;
      cachix.enable = true;
    };
  };

  user.extraGroups = [ "video" ];
}
