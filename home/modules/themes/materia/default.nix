_:
{ config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.themes;
in {
  config = mkIf (cfg.active == "materia") {
    gtk = {
      enable = true;

      theme = {
        name = "Materia-dark-compact";
        package = pkgs.materia-theme;
      };

      iconTheme = {
        name = "kora";
        package = pkgs.kora-icon-theme;
      };

      cursorTheme = {
        name = "Bibata-Modern-Ice";
        size = 16;
        package = pkgs.bibata-cursors;
      };

      font = {
        name = cfg.font.name;
        size = cfg.font.size;
      };
    };

    home.pointerCursor = {
      x11.enable = true;
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Ice";
      size = 16;
      gtk.enable = true;
    };

    xresources.properties = { "Xcursor.theme" = "Bibata-Modern-Ice"; };
  };
}
