{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfgXorg = config.mario.modules.xorg;
  cfgWayland = config.mario.modules.wayland;

  # Creative XOR operator :D
  cfgExclusive =
    (cfgXorg.enable || cfgWayland.enable)
    && (!(cfgXorg.enable && cfgWayland.enable));

  fonts = with pkgs; [
    # Micro$oft
    corefonts
    # Noto
    noto-fonts
    noto-fonts-color-emoji
    noto-fonts-cjk-sans
    # Icons
    font-awesome
    material-design-icons
    # Fira
    fira
    # IBM Plex Mono - Waybar
    ibm-plex
  ];

  social = with pkgs; [telegram-desktop];

  media = with pkgs; [
    pavucontrol
    brightnessctl
    ffmpeg-full
    playerctl
    exiftool
    imagemagick
  ];

  monitoring = with pkgs; [
    btop
    s-tui
  ];

  files = let
    thunarPlugins = with pkgs; [
      xfce.thunar-volman
      xfce.thunar-archive-plugin
      xfce.thunar-media-tags-plugin
    ];
  in
    with pkgs; [
      (xfce.thunar.override {inherit thunarPlugins;})
      xfce.xfconf
      xfce.exo
      mate.engrampa
    ];

  desktop = with pkgs; [
    transmission_4-gtk
  ];
in {
  config = mkIf (cfgXorg.enable || cfgWayland.enable) {
    assertions = [
      {
        assertion = cfgExclusive;
        message = "Can't enable customization for both Xorg and Wayland.";
      }
    ];

    home.packages =
      desktop
      ++ files
      ++ fonts
      ++ monitoring
      ++ media
      ++ social;

    fonts.fontconfig.enable = true;

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
