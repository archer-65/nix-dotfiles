{
  options,
  config,
  lib,
  pkgs,
  outputs,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.greetd;

  greetd-sway-config = pkgs.writeText "greetd-sway-config" ''
    exec "dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP"
    input "type:touchpad" {
      tap enabled
    }
    xwayland disable

    bindsym Mod4+shift+e exec swaynag \
      -t warning \
      -m 'What do you want to do?' \
      -b 'Poweroff' 'systemctl poweroff' \
      -b 'Reboot' 'systemctl reboot'

    exec "${config.programs.regreet.package}/bin/regreet -l debug; swaymsg exit"
  '';
in {
  options.system.modules.graphical.greetd = {
    enable = mkEnableOption "greetd configuration";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # TODO: change theme!
      (catppuccin-gtk.override {
        accents = ["mauve"];
        size = "compact";
        variant = "mocha";
      })
      bibata-cursors
      gnome.adwaita-icon-theme
    ];

    fonts.packages = with pkgs; [
      roboto
    ];

    programs.regreet = {
      enable = true;
      settings = {
        background = {
          path = outputs.wallpapers.nixos-dark.src;
          fit = "Cover";
        };
        GTK = {
          application_prefer_dark_theme = true;
          theme_name = "Adwaita";
          icon_theme_name = "Adwaita";
          cursor_theme_name = "Bibata-Modern-Classic";
          font_name = "Roboto 12";
        };
      };
    };

    services.greetd.settings.default_session.command = "${pkgs.sway}/bin/sway --config ${greetd-sway-config}";
  };
}
