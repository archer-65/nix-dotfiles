{ pkgs, config, ... }:

{
  imports = [
    ./apps
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

  services = { flameshot.enable = true; };

  # home.file.".background-image".source = ../../res/commodore.jpg;
}
