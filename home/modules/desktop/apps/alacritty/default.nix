{ pkgs, ... }:

let fontSize = 14.0;
in {
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
      font = {
        normal = {
          family = "VictorMono Nerd Font";
          style = "Regular";
        };
        bold = {
          family = "VictorMono Nerd Font";
          style = "Bold";
        };
        italic = {
          family = "VictorMono Nerd Font";
          style = "Italic";
        };
        size = fontSize;
      };

      # Colors (must find a better way to handle this)
      # ONEDARK
      colors = {
        primary = {
          background = "#1e2127";
          foreground = "#abb2bf";
          bright_foreground = "#e6efff";
        };
        normal = {
          black = "#1e2127";
          red = "#e06c75";
          green = "#98c379";
          yellow = "#d19a66";
          blue = "#61afef";
          magenta = "#c678dd";
          cyan = "#56b6c2";
          white = "#828791";
        };
        bright = {
          black = "#5c6370";
          red = "#e06c75";
          green = "#98c379";
          yellow = "#d19a66";
          blue = "#61afef";
          magenta = "#c678dd";
          cyan = "#56b6c2";
          white = "#e6efff";
        };
        dim = {
          black = "#1e2127";
          red = "#e06c75";
          green = "#98c379";
          yellow = "#d19a66";
          blue = "#61afef";
          magenta = "#c678dd";
          cyan = "#56b6c2";
          white = "#828791";
        };
      };

      selection.save_to_clipboard = true;
      live_config_reload = true;
    };
  };
}
