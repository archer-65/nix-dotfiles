{
  pkgs,
  config,
  lib,
  options,
  ...
}:
with lib; let
  cfgXorg = config.home.modules.desktop.xorg;
  cfgWayland = config.home.modules.desktop.wayland;

  # Creative XOR operator :D
  cfgExclusive =
    (cfgXorg.enable || cfgWayland.enable)
    && (!(cfgXorg.enable && cfgWayland.enable));

  # Overriding nerd fonts (if you don't, all nerd fonts will be installed.)
  nerdFonts = pkgs.nerdfonts.override {
    fonts = ["FiraCode" "JetBrainsMono" "VictorMono" "Iosevka"];
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

  socialPkgs = with pkgs; [tdesktop];

  mediaPkgs = with pkgs; [pavucontrol];

  utilPkgs = with pkgs; [neofetch];

  monitorPkgs = with pkgs; [btop s-tui];

  desktopPkgs = with pkgs;
    [
      brightnessctl
      ffmpeg-full
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
    ]
    ++ userFonts;
in {
  config = {
    assertions = [
      {
        assertion = cfgExclusive;
        message = "Can't enable customization for both Xorg and Wayland.";
      }
    ];

    programs.home-manager.enable = true;

    home.packages =
      [
        pkgs.scripts.volume
        pkgs.scripts.hwmon_devices
        pkgs.ispell
        pkgs.exiftool
        pkgs.imagemagick
        pkgs.transmission-gtk
        pkgs.appimage-run
      ]
      ++ socialPkgs
      ++ mediaPkgs
      ++ utilPkgs
      ++ monitorPkgs
      ++ desktopPkgs;

    fonts.fontconfig.enable = true;

    xsession.enable = true;

    systemd.user.services = {
      polkit = {
        Unit = {
          Description = "polkit-gnome";
          Documentation = ["man:polkit(8)"];
          PartOf = ["graphical-session.target"];
        };
        Service = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          RestartSec = 3;
          Restart = "always";
        };
        Install = {WantedBy = ["graphical-session.target"];};
      };
    };
  };
}
