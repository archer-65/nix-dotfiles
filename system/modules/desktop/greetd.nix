_:
{ options, config, lib, pkgs, ... }:

with lib;

let
  cfg = config.modules.desktop.greetd;

  inherit (config.dotfiles) assetsDir;
  background = "${assetsDir}/greeter.png";

  swayConfig = pkgs.writeText "greetd-sway-config" ''
    # `-l` activates layer-shell mode. Notice that `swaymsg exit` will run after gtkgreet.
    exec "${pkgs.greetd.gtkgreet}/bin/gtkgreet -l -c sway -s /etc/greetd/gtkgreet.css; swaymsg exit"
    bindsym Mod4+shift+e exec swaynag \
      -t warning \
      -m 'What do you want to do?' \
      -b 'Poweroff' 'systemctl poweroff' \
      -b 'Reboot' 'systemctl reboot'

    include /etc/sway/config.d/*
  '';
in {
  options.modules.desktop.greetd = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    # We need to disable `lightdm` because it is enabled by default.
    services.xserver.displayManager.lightdm.enable = false;

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
        sway
        Hyprland
        qtile start
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
