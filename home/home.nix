{ pkgs, ... }:

let
  socialPkgs = with pkgs; [
    tdesktop
    discord
  ];

  mediaPkgs = with pkgs; [
    mpv
    pavucontrol
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

  haskellPkgs = with pkgs.haskellPackages; [
    greenclip
  ];

  desktopOnlyPkgs = with pkgs; [
    corectrl
  ];
in {
  imports = [
    ./programs
    ./desktop
    ./services
  ];

  programs.home-manager.enable = true;

  home = {
    packages = socialPkgs ++ mediaPkgs ++ coreUtilPkgs ++ utilPkgs ++ monitorPkgs ++ haskellPkgs;
  };
}
