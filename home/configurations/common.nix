{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfgXorg = config.mario.modules.desktop.xorg;
  cfgWayland = config.mario.modules.desktop.wayland;

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

  mediaPkgs = with pkgs; [
    pavucontrol
    brightnessctl
    ffmpeg-full
    playerctl
    ispell
    exiftool
    imagemagick
  ];

  archivePkgs = with pkgs; [
    zip
    unzip
    unrar
  ];

  monitorPkgs = with pkgs; [btop s-tui neofetch];

  xfcePkgs = with pkgs; [
    xfce.xfconf
    xfce.exo
    (xfce.thunar.override
      {
        thunarPlugins = with pkgs; [
          xfce.thunar-volman
          xfce.thunar-archive-plugin
          xfce.thunar-media-tags-plugin
        ];
      })
  ];

  scriptsPkgs = with pkgs; [
    script-volume
    script-hwmon_devices
  ];

  desktopPkgs = with pkgs; [
    mate.engrampa
    transmission-gtk
    appimage-run
  ];
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
      desktopPkgs
      ++ xfcePkgs
      ++ userFonts
      ++ archivePkgs
      ++ monitorPkgs
      ++ mediaPkgs
      ++ socialPkgs
      ++ scriptsPkgs;

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
