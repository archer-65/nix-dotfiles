{ pkgs, ... }:

let
  # WM packages
  workspacePkgs = with pkgs; [
    nitrogen
    rofi
    rofi-emoji
    playerctl
    betterlockscreen
    flameshot
    xfce.thunar
    xfce.thunar-volman
    xfce.thunar-archive-plugin
  ];

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
  imports = (import ./programs)

  programs.home-manager.enable = true;

  home = {
    packages = workspacePkgs ++ socialPkgs ++ mediaPkgs ++ coreUtilPkgs ++ utilPkgs ++ monitorPkgs ++ haskellPkgs;
  };
  
  services = {
    picom = {
      enable = true;
      package = pkgs.picom.overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          repo = "picom";
          owner = "ibhagwan";
	        rev = "next-rebase";
          sha256 = "JNbP8NZkHjlcQjSRfPbHdpTFrPO6GSZQ5qzDb0yi+pI=";
        };
      });
    };

    flameshot.enable = true;
  };

  programs = {
    bash = {
      enable = true;
      #enableCompletion = true;
    };

    starship = {
      enable = true;
    };

    autorandr = {
      enable = true;
    };
  };
}
