{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.term.alacritty;
  cfgTheme = config.mario.modules.themes;
in {
  options.mario.modules.term.alacritty = {
    enable = mkEnableOption "alacritty configuration";
  };

  config = mkIf cfg.enable {
    stylix.targets.alacritty.enable = true;
    stylix.targets.alacritty.fonts.enable = false;
    stylix.targets.alacritty.opacity.enable = false;

    programs.alacritty = {
      enable = true;

      settings = {
        # Window section settings
        window = {
          padding = {
            x = 20;
            y = 15;
          };
          decorations =
            if pkgs.stdenv.isDarwin
            then "Full"
            else "None";
          opacity = 0.95;
          option_as_alt = "OnlyLeft";
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

        selection.save_to_clipboard = true;
        general.live_config_reload = true;
      };
    };
  };
}
