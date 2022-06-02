_:
{ pkgs, config, lib, ... }:

let
  socialPkgs = with pkgs; [ tdesktop discord ];

  mediaPkgs = with pkgs; [ mpv pavucontrol pamixer pasystray ];

  coreUtilPkgs = with pkgs; [ exa bat ripgrep fd ];

  utilPkgs = with pkgs; [ rbw ];

  monitorPkgs = with pkgs; [ btop s-tui ];

  desktopOnlyPkgs = with pkgs; [ corectrl ];

  qt5Pkgs = with pkgs.libsForQt5; [ qtstyleplugin-kvantum breeze-qt5 ];
in rec {
  imports = [ ./desktop ];

  programs.home-manager.enable = true;

  home.packages = #[ pkgs.scripts.volume ] ++
    socialPkgs ++ mediaPkgs
    ++ coreUtilPkgs ++ utilPkgs ++ monitorPkgs ++ qt5Pkgs;

  home.sessionVariables = { QT_QPA_PLATFORMTHEME = "qt5ct"; };
}
