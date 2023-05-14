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
  rofiFonts = pkgs.stable.nerdfonts.override {
    fonts = ["Iosevka"];
  };
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
      plugins = with pkgs; [rofi-emoji];
    };

    home.packages = with pkgs;
      [rofi-powermenu]
      ++ optionals cfgBitwarden.enable [rofi-rbw]
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
