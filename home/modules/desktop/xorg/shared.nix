{ pkgs, ... }:

{
  home.packages = with pkgs; [
    nitrogen
    xclip
    xdotool
  ];

  services = {
    flameshot.enable = true;
  };

  user-modules.desktop = {
    apps = { greenclip.enable = true; };
    services = { locker.enable = true; };
  };
}
