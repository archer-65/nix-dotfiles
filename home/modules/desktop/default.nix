# Thank you hlissner!
{ config, options, lib, pkgs, ... }:

with lib;
let
  # These functions should stay in another place, but right now I need them only here.
  attrsToList = attrs:
    mapAttrsToList (name: value: { inherit name value; }) attrs;
  countAttrs = pred: attrs:
    count (attr: pred attr.name attr.value) (attrsToList attrs);

  cfgXorg = config.user-modules.desktop.xorg;
  cfgWayland = config.user-modules.desktop.wayland;

  # Creative XOR operator :D
  cfgExclusive = ((cfgXorg.enable || cfgWayland.enable)
                  && (!(cfgXorg.enable && cfgWayland.enable)));
in {
  config = {
    assertions = [
      {
        assertion = cfgExclusive;
        message = "Can't enable customization for both Xorg and Wayland.";
      }
      {
        assertion = (countAttrs (n: v: n == "enable" && v) cfgXorg) < 2;
        message = "[Xorg] Can't have more than one WM/DE enable at a time.";
      }
      {
        assertion = (countAttrs (n: v: n == "enable" && v) cfgWayland) < 2;
        message = "[Wayland] Can't have more than one WM/DE enable at a time.";
      }
    ];

    home.packages = with pkgs; [
      brightnessctl
      playerctl
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
      zip
      unzip
    ];

    xsession.enable = true;

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
  };
}
