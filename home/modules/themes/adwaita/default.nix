{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.user-modules.themes;
in {
  config = mkIf (cfg.active == "adwaita") {
    gtk = {
      enable = true;

      theme = {
        name =
          if cfg.darkTheme
          then "adw-gtk3-dark"
          else "adw-gtk3";
        package = pkgs.adw-gtk3;
      };

      iconTheme = {
        name = "Adwaita";
        package = pkgs.gnome.adwaita-icon-theme;
      };

      cursorTheme = {
        name = "Bibata-Modern-Ice";
        size = 16;
        package = pkgs.bibata-cursors;
      };

      font = {
        inherit (cfg.font) name;
        inherit (cfg.font) size;
      };
    };

    home.pointerCursor = {
      x11.enable = true;
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Ice";
      size = 16;
      gtk.enable = true;
    };

    xresources.properties = {"Xcursor.theme" = "Bibata-Modern-Ice";};
  };
}
