{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.desktop.apps.rofi;
  cfgWayland = config.home.modules.desktop.wayland;
  inherit (config.dotfiles) configDir;

  rbw =
    if config.home.modules.credentials.bitwarden.enable
    then [pkgs.rofi-rbw]
    else [];

  utils = with pkgs; [
    script-usedcpu
    script-usedram
    script-powermenu
    script-launcher
    script-emoji
  ];
in {
  options.home.modules.desktop.apps.rofi = {
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

    xdg.configFile."rofi/colors" = {
      source = "${configDir}/rofi/colors";
      recursive = true;
    };

    xdg.configFile."rofi/themes" = {
      source = "${configDir}/rofi/themes";
      recursive = true;
    };

    xdg.configFile."rofi-rbw.rc".source = "${configDir}/rofi-rbw.rc";

    home.packages = with pkgs; [rofi-emoji] ++ rbw ++ utils;
  };
}
