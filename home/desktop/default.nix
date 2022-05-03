{ pkgs, ... }: 

{
  home.packages = with pkgs; [
    betterlockscreen
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
          sha256 = "JNbP8NZkHjlcQjSRfPbHdpTFrPO6GSZQ5qzDb0yi+pI=";
        };
      });
    };

    flameshot.enable = true;
  };

  xdg.configFile = {
    source = "./qtile";
    recursive = "true";
  };
}