{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.greetd;

  inherit (config.dotfiles) assetsDir;
  background = "${assetsDir}/greeter.png";
in {
  options.system.modules.graphical.greetd = {
    enable = mkEnableOption "greetd configuration";
  };

  config = mkIf cfg.enable {
    services.greetd = {
      enable = true;
      settings = {
        default_session.command = let
          greetd-gtk-style = pkgs.writeText "greetd-gtk-style" ''
            window {
              background-image: url("file://${background}");
              background-size: cover;
              background-position: center;
            }
          '';

          greetd-sway-config = pkgs.writeText "greetd-sway-config" ''
            exec "${pkgs.greetd.gtkgreet}/bin/gtkgreet -l -s ${greetd-gtk-style}; swaymsg exit"

            bindsym Mod4+shift+e exec swaynag \
              -t warning \
              -m 'What do you want to do?' \
              -b 'Poweroff' 'systemctl poweroff' \
              -b 'Reboot' 'systemctl reboot'

            exec dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP
          '';
        in "${pkgs.sway}/bin/sway --config ${greetd-sway-config}";
      };
    };

    environment.etc = {
      # Definition of greetd environments
      "greetd/environments".text = ''
        Hyprland
        sway
        bash
      '';
    };
  };
}
