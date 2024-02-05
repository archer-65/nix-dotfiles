{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.media.documents;
  cfgTheme = config.mario.modules.themes;
  inherit (config.colorScheme) palette;
in {
  options.mario.modules.media.documents = {
    zathura.enable = mkEnableOption "pdf support (zathura)";
    okular.enable = mkEnableOption "pdf support (okular)";
  };

  config = mkMerge [
    (mkIf cfg.zathura.enable {
      programs.zathura = {
        enable = true;
        options = {
          font =
            "${cfgTheme.font.term.family} "
            + (toString cfgTheme.font.term.size);

          default-bg = "#${palette.base00}";
          default-fg = "#${palette.base01}";

          statusbar-fg = "#${palette.base04}";
          statusbar-bg = "#${palette.base02}";

          inputbar-bg = "#${palette.base00}";
          inputbar-fg = "#${palette.base07}";

          notification-bg = "#${palette.base00}";
          notification-fg = "#${palette.base07}";

          notification-error-bg = "#${palette.base00}";
          notification-error-fg = "#${palette.base06}";

          notification-warning-bg = "#${palette.base00}";
          notification-warning-fg = "#${palette.base06}";

          highlight-color = "#${palette.base0A}";
          highlight-active-color = "#${palette.base0D}";

          completion-bg = "#${palette.base01}";
          completion-fg = "#${palette.base0D}";

          recolor-lightcolor = "#${palette.base00}";
          recolor-darkcolor = "#${palette.base06}";

          recolor = "false";
          recolor-keephue = "false";
        };
      };
    })

    (mkIf cfg.okular.enable {home.packages = with pkgs; [okular];})
  ];
}
