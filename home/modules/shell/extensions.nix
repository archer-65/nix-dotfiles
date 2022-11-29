{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.extensions;
  cfgBash = config.mario.modules.shell.bash;
  inherit (config.colorScheme) colors;
in {
  options.mario.modules.shell.extensions = {
    enable = mkEnableOption "shell useful commands (e.g. bat, exa) ";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.bat = {
        enable = true;
        config = {
          theme = "base16";
        };
      };

      programs.exa = {
        enable = true;
        enableAliases = true;
      };

      programs.fzf = {
        enable = true;
        colors = {
          "bg+" = "#${colors.base01}";
          "bg" = "#${colors.base00}";
          "spinner" = "#${colors.base0C}";
          "hl" = "#${colors.base0D}";

          "fg" = "#${colors.base04}";
          "header" = "#${colors.base0D}";
          "info" = "#${colors.base0A}";
          "pointer" = "#${colors.base0C}";

          "marker" = "#${colors.base0C}";
          "fg+" = "#${colors.base06}";
          "prompt" = "#${colors.base0A}";
          "hl+" = "#${colors.base0D}";
        };
      };

      programs.nix-index = {
        enable = true;
        enableBashIntegration = true;
      };

      home.packages = with pkgs; [bat-extras.batman fd ripgrep];
    }

    # Useful aliases for our shells
    (mkIf cfgBash.enable {
      programs.bash.shellAliases = {
        cat = "${pkgs.bat}/bin/bat";
      };
    })
  ]);
}
