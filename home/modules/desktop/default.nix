{ pkgs, ... }:

{
  home.packages = with pkgs; [
    brightnessctl
    nitrogen
    playerctl
    xclip
    xdotool
    xfce.xfconf
    (xfce.thunar.override {
      thunarPlugins = with pkgs; [
        xfce.thunar-volman
        xfce.thunar-archive-plugin
        xfce.thunar-media-tags-plugin
      ];
    })
  ];

  services = { flameshot.enable = true; };
  user-modules.desktop = {
    apps = { greenclip.enable = true; };
    services = { locker.enable = true; };
  };
  # home.file.".background-image".source = ../../res/commodore.jpg;
}
