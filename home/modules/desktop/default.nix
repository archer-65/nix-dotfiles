# Thank you hlissner!
{ config, options, lib, pkgs, ... }:

with lib;
let
  cfgXorg = config.user-modules.desktop.xorg;
  cfgWayland = config.user-modules.desktop.wayland;

  # Creative XOR operator :D
  cfgExclusive = (cfgXorg.enable || cfgWayland.enable)
                 && (!(cfgXorg.enable && cfgWayland.enable));

  # Overriding nerd fonts (if you don't, all nerd fonts will be installed.)
  nerdFonts = pkgs.nerdfonts.override {
    fonts = [ "FiraCode" "JetBrainsMono" "VictorMono" "Iosevka" ];
  };

  userFonts = with pkgs; [
    nerdFonts
    corefonts
    source-code-pro
    roboto
    noto-fonts
    noto-fonts-emoji
    noto-fonts-cjk-sans
    font-awesome
    material-design-icons
  ];
in {
  config = {
    assertions = [
      {
        assertion = cfgExclusive;
        message = "Can't enable customization for both Xorg and Wayland.";
      }
    ];

    fonts.fontconfig.enable = true;

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
      unrar
    ] ++ userFonts;

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
