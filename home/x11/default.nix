{ pkgs, ... }: 

{
  imports = [
    ./autorandr.nix
    ./dunst.nix
    ./picom.nix
  ]

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
    flameshot.enable = true;
  };

  xdg.configFile."qtile" = {
    source = ./qtile;
    recursive = true;
  };

  #home.file.".background-image".source = ../../res/commodore.jpg;
}
