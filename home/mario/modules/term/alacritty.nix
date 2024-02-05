{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.term.alacritty;
  cfgTheme = config.mario.modules.themes;
  inherit (config.colorScheme) palette;
in {
  options.mario.modules.term.alacritty = {
    enable = mkEnableOption "alacritty configuration";
  };

  config = mkIf cfg.enable {
    programs.alacritty = {
      enable = true;

      settings = {
        # Window section settings
        window = {
          padding = {
            x = 20;
            y = 15;
          };
          decorations = "none";
          opacity = 0.95;
        };

        # Scrolling history and multiplier
        scrolling = {
          history = 1000;
          multiplier = 3;
        };

        # Font configuration
        font = let
          inherit (cfgTheme.font.term) family;
          inherit (cfgTheme.font.term) size;
        in {
          normal = {
            inherit family;
            style = "Regular";
          };
          bold = {
            inherit family;
            style = "Bold";
          };
          italic = {
            inherit family;
            style = "Italic";
          };
          inherit size;
        };

        colors = {
          primary = {
            background = "#${palette.base00}";
            foreground = "#${palette.base05}";
          };
          normal = {
            black = "#${palette.base00}";
            red = "#${palette.base08}";
            green = "#${palette.base0B}";
            yellow = "#${palette.base09}";
            blue = "#${palette.base0D}";
            magenta = "#${palette.base0E}";
            cyan = "#${palette.base0C}";
            white = "#${palette.base05}";
          };
          bright = {
            black = "#${palette.base03}";
            red = "#${palette.base06}";
            green = "#${palette.base0B}";
            yellow = "#${palette.base09}";
            blue = "#${palette.base0D}";
            magenta = "#${palette.base0E}";
            cyan = "#${palette.base0C}";
            white = "#${palette.base05}";
          };
        };

        selection.save_to_clipboard = true;
        live_config_reload = true;
      };
    };
  };
}
