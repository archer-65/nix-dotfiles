{ pkgs, config, user, ... }:

let
  scripts = pkgs.callPackage ./scripts { inherit config pkgs; };

  socialPkgs = with pkgs; [
    tdesktop
    discord
  ];

  mediaPkgs = with pkgs; [
    mpv
    pavucontrol
    pamixer
    pasystray
  ];

  coreUtilPkgs = with pkgs; [
    exa
    bat
    ripgrep
    fd
  ];

  utilPkgs = with pkgs; [
    rbw
  ];

  monitorPkgs = with pkgs; [
    btop
    s-tui
  ];
  
  desktopOnlyPkgs = with pkgs; [
    corectrl
  ];
in rec {
  imports = [
    ./editors
    ./programs
    ./x11
    ./services
    ./themes
  ];

  programs.home-manager.enable = true;

  home = {
    packages = scripts ++ socialPkgs ++ mediaPkgs ++ coreUtilPkgs ++ utilPkgs ++ monitorPkgs;
  };

  home.homeDirectory = "/home/${user}";
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
}
