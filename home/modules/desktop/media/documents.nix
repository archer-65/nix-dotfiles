_:
{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.desktop.media.documents;
in {
  options.user-modules.desktop.media.documents = {
    zathura.enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };

    okular.enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkMerge [
    (mkIf cfg.zathura.enable {
      programs.zathura = {
        enable = true;
        options = {
          default-bg = "#191724";
          default-fg = "#e0def4";

          statusbar-fg = "#e0def4";
          statusbar-bg = "#555169";

          inputbar-bg = "#6e6a86";
          inputbar-fg = "#ebbcba";

          notification-bg = "#e0def4";
          notification-fg = "#555169";

          notification-error-bg = "#f6c177";
          notification-error-fg = "#555169";

          notification-warning-bg = "#ebbcba";
          notification-warning-fg = "#555169";

          highlight-color = "#ebbcba";
          highlight-active-color = "#eb6f92";

          completion-bg = "#6e6a86";
          completion-fg = "#ebbcba";

          completion-highlight-fg = "#26233a";
          completion-highlight-bg = "#ebbcba";

          recolor-lightcolor = "#191724";
          recolor-darkcolor = "#e0def4";

          recolor = "false";
          recolor-keephue = "false";
        };
      };
    })

    (mkIf cfg.okular.enable { home.packages = with pkgs; [ okular ]; })
  ];
}
