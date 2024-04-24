# Work laptop laptop
{
  pkgs,
  lib,
  ...
}: {
 
  programs.sway.package = pkgs.sway-displaylink;

  system.modules = {
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      monitoring.enable = true;
      qmk.enable = true;
    };

    dev = {
      adb.enable = true;
      docker.enable = true;
      manpages.enable = true;
    };

    credentials = {
      ssh.enable = true;
      gpg.enable = true;
      yubikey.enable = true;
    };

    graphical = {
      displaylink.enable = true;
      wayland.enable = true;
      greetd.enable = true;
    };

    core = {
      boot.quietboot.enable = true;
      cachix.enable = true;
      nix-ld.enable = true;
    };

    shell = {
      bash.enable = true;
      zsh.enable = true;
    };
  };

  primaryUser.shell = pkgs.zsh;

  services.flatpak.enable = true;
}
