{ pkgs, config, ... }: 

{
  imports = [
    ./autorandr.nix
    ./dunst.nix
    ./picom.nix
    ./rofi
    ./greenclip
  ];

  home.packages = with pkgs; [
    betterlockscreen
    brightnessctl
    xss-lock
    nitrogen
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

  xsession = {
    enable = true;
  };

  # home.x11.enable = true;

  home.pointerCursor = {
    x11.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Ice";
    size = 16;
  };

  xresources.properties = {
    "Xcursor.theme" = "Bibata-Modern-Ice";
  };

  #home.file.".background-image".source = ../../res/commodore.jpg;
}
