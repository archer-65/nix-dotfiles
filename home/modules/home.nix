_: { pkgs, config, lib, ... }:

let
  scripts = pkgs.callPackage ./scripts { inherit config pkgs; };

  socialPkgs = with pkgs; [ tdesktop discord ];

  mediaPkgs = with pkgs; [ mpv pavucontrol pamixer pasystray ];

  coreUtilPkgs = with pkgs; [ exa bat ripgrep fd ];

  utilPkgs = with pkgs; [ rbw ];

  monitorPkgs = with pkgs; [ btop s-tui ];

  desktopOnlyPkgs = with pkgs; [ corectrl ];

  qt5Pkgs = with pkgs.libsForQt5; [ qtstyleplugin-kvantum breeze-qt5 ];

in rec {
  imports = [
    #./editors
    #./programs
    #./x11
    ./desktop
    #./themes
  ];

  programs.home-manager.enable = true;

  home = {
    packages = scripts ++ socialPkgs ++ mediaPkgs ++ coreUtilPkgs ++ utilPkgs
      ++ monitorPkgs ++ qt5Pkgs;
  };

  #home.homeDirectory = "/home/${user}";
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    desktop = "${home.homeDirectory}/desktop";
    documents = "${home.homeDirectory}/docs";
    download = "${home.homeDirectory}/dl";
    music = "${home.homeDirectory}/music";
    pictures = "${home.homeDirectory}/pics";
    publicShare = "${home.homeDirectory}/public";
    templates = "${home.homeDirectory}/templates";
    videos = "${home.homeDirectory}/videos";
  };

  home.sessionVariables = {
    # VISUAL = "emacsclient -c -a emacs";
    # EDITOR = "emacsclient -t";
    QT_QPA_PLATFORMTHEME = "qt5ct";
  };
}
