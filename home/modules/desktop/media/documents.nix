{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.desktop.media.documents;
  cfgTheme = config.mario.modules.themes;
  inherit (config.colorScheme) colors;
in {
  options.mario.modules.desktop.media.documents = {
    zathura.enable = mkEnableOption "pdf support (zathura)";

    okular.enable = mkEnableOption "pdf support (okular)";
  };

  config = mkMerge [
    (mkIf cfg.zathura.enable {
      programs.zathura = {
        enable = true;
        options = {
          font =
            "${cfgTheme.font.term.name} "
            + (toString cfgTheme.font.term.size);

          default-bg = "#${colors.base00}";
          default-fg = "#${colors.base01}";

          statusbar-fg = "#${colors.base04}";
          statusbar-bg = "#${colors.base02}";

          inputbar-bg = "#${colors.base00}";
          inputbar-fg = "#${colors.base07}";

          notification-bg = "#${colors.base00}";
          notification-fg = "#${colors.base07}";

          notification-error-bg = "#${colors.base00}";
          notification-error-fg = "#${colors.base06}";

          notification-warning-bg = "#${colors.base00}";
          notification-warning-fg = "#${colors.base06}";

          highlight-color = "#${colors.base0A}";
          highlight-active-color = "#${colors.base0D}";

          completion-bg = "#${colors.base01}";
          completion-fg = "#${colors.base0D}";

          recolor-lightcolor = "#${colors.base00}";
          recolor-darkcolor = "#${colors.base06}";

          recolor = "false";
          recolor-keephue = "false";
        };
      };
    })

    (mkIf cfg.okular.enable {home.packages = with pkgs; [okular];})
  ];
}
