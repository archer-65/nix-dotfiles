{ lib, config, pkgs, ... }:

{ 
  # Global services
  services = {
    # SSH and SSH Daemon
    openssh = {
      enable = true;
    };
    sshd.enable = true;

    # Pipewire settings
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    # Flatpak settings
    flatpak.enable = true;

    # Enable CUPS
    printing = {
      enable = true;
      drivers = [ pkgs.hplip ];
    };
  };
}