_:
{ pkgs, ... }:

let
  socialPkgs = with pkgs; [ discord tdesktop ];

  mediaPkgs = with pkgs; [ mpv pavucontrol ];

  utilPkgs = with pkgs; [ rbw neofetch ];

  monitorPkgs = with pkgs; [ btop s-tui ];

  qt5Pkgs = with pkgs.libsForQt5; [ qtstyleplugin-kvantum breeze-qt5 ];
in rec {
  imports = [ ./desktop ];

  programs.home-manager.enable = true;

  home.packages = [
    pkgs.scripts.volume
    pkgs.firefox
    pkgs.ispell
    pkgs.exiftool
    pkgs.imagemagick
    pkgs.transmission-gtk
  ] ++ socialPkgs ++ mediaPkgs ++ utilPkgs ++ monitorPkgs ++ qt5Pkgs;

  # programs.firefox = {
  #   enable = true;
  #   package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
  #     forceWayland = true;
  #   };
  # };
}
