{ pkgs, ... }:

{
  home.packages = with pkgs; [
    #betterlockscreen
    brightnessctl
    #xss-lock
    nitrogen
    playerctl
    xclip
    xdotool
    xfce.thunar
    xfce.thunar-volman
    xfce.thunar-archive-plugin
  ];

  services = { flameshot.enable = true; };

  user-modules.desktop.services = { locker.enable = true; };
  # home.file.".background-image".source = ../../res/commodore.jpg;
}
