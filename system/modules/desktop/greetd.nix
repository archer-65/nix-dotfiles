{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.desktop.greetd;

  inherit (config.dotfiles) assetsDir;
  background = "${assetsDir}/greeter.png";

  swayConfig = pkgs.writeText "greetd-sway-config" ''
    exec "${pkgs.greetd.gtkgreet}/bin/gtkgreet -l -s /etc/greetd/gtkgreet.css; swaymsg exit"

    bindsym Mod4+shift+e exec swaynag \
      -t warning \
      -m 'What do you want to do?' \
      -b 'Poweroff' 'systemctl poweroff' \
      -b 'Reboot' 'systemctl reboot'

    exec dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP
  '';
in {
  options.system.modules.desktop.greetd = {
    enable = mkEnableOption "greetd configuration";
  };

  config = mkIf cfg.enable {
    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.sway}/bin/sway --config ${swayConfig}";
        };
      };
    };

    environment.etc = {
      # Definition of greetd environments
      "greetd/environments".text = ''
        Hyprland
        sway
        qtile start
        bash
      '';

      # Greeter style
      "greetd/gtkgreet.css".text = ''
        window {
         background-image: url("file://${background}");
         background-size: cover;
         background-position: center;
        }
      '';
    };
  };
}
