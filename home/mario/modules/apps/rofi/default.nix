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
  cfgBitwarden = config.mario.modules.credentials.bitwarden;

  rofiFonts = pkgs.nerd-fonts.iosevka;
in {
  options.mario.modules.apps.rofi = {
    enable = mkEnableOption "rofi configuration";
  };

  config = mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      package = pkgs.rofi;
      plugins = [pkgs.rofi-emoji];
    };

    home.packages = with pkgs;
      [pkgs.rofi-powermenu]
      ++ optionals cfgBitwarden.enable [pkgs.rofi-rbw]
      ++ [rofiFonts];

    xdg.configFile."rofi/colors/color.rasi".text = ''
      /*
       *
       * Change here you colorscheme
       *
       */

       @import "${config.colorscheme.slug}.rasi"
    '';

    xdg.configFile."rofi/colors" = {
      source = ./config/colors;
      recursive = true;
    };

    xdg.configFile."rofi/themes" = {
      source = ./config/themes;
      recursive = true;
    };

    xdg.configFile."rofi-rbw.rc" = mkIf cfgBitwarden.enable {
      text = ''
        action = type
        prompt = Select credentials
        selector-args = -theme ~/.config/rofi/themes/rbw
      '';
    };
  };
}
