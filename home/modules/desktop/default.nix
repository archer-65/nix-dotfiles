{ pkgs, ... }:

{
  home.packages = with pkgs; [
    brightnessctl
    nitrogen
    playerctl
    xclip
    xdotool
    xfce.xfconf
    xfce.exo
    (xfce.thunar.override {
      thunarPlugins = with pkgs; [
        xfce.thunar-volman
        xfce.thunar-archive-plugin
        xfce.thunar-media-tags-plugin
      ];
    })
    mate.engrampa
    unzip
  ];

  services = { flameshot.enable = true; };
  user-modules.desktop = {
    apps = { greenclip.enable = true; };
    services = { locker.enable = true; };
  };

  systemd.user.services = {
    polkit = {
      Unit = {
        Description = "polkit-gnome";
        Documentation = [ "man:polkit(8)" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart =
          "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        RestartSec = 3;
        Restart = "always";
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };
}
