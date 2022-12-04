{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.term.alacritty;
  cfgTerm = config.mario.modules.term;
  inherit (config.colorScheme) colors;
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
            y = 20;
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
          family = cfgTerm.font.family;
          size = cfgTerm.font.size;
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
            background = "#${colors.base00}";
            foreground = "#${colors.base05}";
          };
          normal = {
            black = "#${colors.base00}";
            red = "#${colors.base08}";
            green = "#${colors.base0B}";
            yellow = "#${colors.base09}";
            blue = "#${colors.base0D}";
            magenta = "#${colors.base0E}";
            cyan = "#${colors.base0C}";
            white = "#${colors.base05}";
          };
          bright = {
            black = "#${colors.base03}";
            red = "#${colors.base06}";
            green = "#${colors.base0B}";
            yellow = "#${colors.base09}";
            blue = "#${colors.base0D}";
            magenta = "#${colors.base0E}";
            cyan = "#${colors.base0C}";
            white = "#${colors.base05}";
          };
        };

        selection.save_to_clipboard = true;
        live_config_reload = true;
      };
    };
  };
}
