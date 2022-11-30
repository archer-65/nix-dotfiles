{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.apps.rofi;
  cfgWayland = config.mario.modules.wayland;
  inherit (config.dotfiles) configDir;

  rbw = optionals config.mario.modules.credentials.bitwarden.enable [pkgs.rofi-rbw];

  utils = with pkgs; [
    script-usedcpu
    script-usedram
    script-powermenu
    script-launcher
    script-emoji
  ];
in {
  options.mario.modules.apps.rofi = {
    enable = mkEnableOption "rofi configuration";
  };

  config = mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      package =
        if cfgWayland.enable
        then pkgs.rofi-wayland
        else pkgs.rofi;
      plugins = with pkgs; [rofi-emoji] ++ rbw;
    };

    home.packages = with pkgs; [rofi-emoji] ++ rbw ++ utils;

    xdg.configFile."rofi/colors/color.rasi".text = ''
      /*
       *
       * Change here you colorscheme
       *
       */

       @import "${config.colorscheme.slug}.rasi"
    '';

    xdg.configFile."rofi/colors" = {
      source = "${configDir}/rofi/colors";
      recursive = true;
    };

    xdg.configFile."rofi/themes" = {
      source = "${configDir}/rofi/themes";
      recursive = true;
    };

    xdg.configFile."rofi-rbw.rc".source = "${configDir}/rofi-rbw.rc";
  };
}
