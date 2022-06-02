_:
{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.user-modules.desktop.apps.rofi;
  configDir = config.dotfiles.configDir;
in {
  options.user-modules.desktop.apps.rofi = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.rofi = {
      enable = true;

      plugins = with pkgs; [ rofi-emoji rofi-rbw ];
    };

    xdg.configFile."rofi/colors" = {
      # source = ../../../../config/rofi/colors;
      source = "${configDir}/rofi/colors";
      recursive = true;
    };

    xdg.configFile."rofi/themes" = {
      # source = ../../../../config/rofi/themes;
      source = "${configDir}/rofi/themes";
      recursive = true;
    };

    xdg.configFile."rofi-rbw.rc".source = "${configDir}/rofi-rbw.rc";

    home.packages = with pkgs; [
      rofi-rbw
      rofi-emoji

      scripts.usedcpu
      scripts.usedram
      scripts.rofi.powermenu
      scripts.rofi.launcher
      scripts.rofi.greenclip
      scripts.rofi.emoji
    ];
  };
}
