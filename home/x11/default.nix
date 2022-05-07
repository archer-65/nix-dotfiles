{ pkgs, ... }: 

{
  home.packages = with pkgs; [
    betterlockscreen
    brightnessctl
    xss-lock
    nitrogen
    rofi
    rofi-emoji
    playerctl
    pamixer
    xclip
    libnotify
    xfce.thunar
    xfce.thunar-volman
    xfce.thunar-archive-plugin
  ];

  services = {
    picom = {
      enable = true;
      package = pkgs.picom.overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          repo = "picom";
          owner = "ibhagwan";
	        rev = "next-rebase";
          sha256 = "1hVFBGo4Ieke2T9PqMur1w4D0bz/L3FAvfujY9Zergw=";
        };
      });
    };

    flameshot.enable = true;
  };

  xdg.configFile."qtile" = {
    source = ./qtile;
    recursive = true;
  };

  home.file.".background-image".source = ../../res/commodore.jpg;
}
