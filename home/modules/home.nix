_:
{ pkgs, ... }:

let
  socialPkgs = with pkgs; [ tdesktop ];

  mediaPkgs = with pkgs; [ pavucontrol ];

  utilPkgs = with pkgs; [ rbw neofetch ];

  monitorPkgs = with pkgs; [ btop s-tui ];

in rec {

  imports = [ ./desktop ];

  programs.home-manager.enable = true;

  home.packages = [
    pkgs.scripts.volume
    pkgs.scripts.hwmon_devices
    pkgs.ispell
    pkgs.exiftool
    pkgs.imagemagick
    pkgs.transmission-gtk
    pkgs.appimage-run
  ] ++ socialPkgs ++ mediaPkgs ++ utilPkgs ++ monitorPkgs;
}
