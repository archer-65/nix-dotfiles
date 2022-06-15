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

  home.packages = [ pkgs.scripts.volume pkgs.firefox pkgs.ispell ] ++ socialPkgs
    ++ mediaPkgs ++ utilPkgs ++ monitorPkgs ++ qt5Pkgs;
}
