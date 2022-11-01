{
  config,
  lib,
  pkgs,
  nix-colors,
  ...
}:
with lib; let
  cfg = config.home.modules.themes;
  theme =
    if cfg.darkTheme
    then "Materia-dark-compact"
    else "Materia-light-compact";
  icons =
    if cfg.darkTheme
    then "kora"
    else "kora-light-panel";
  cursor =
    if cfg.darkTheme
    then "Bibata-Modern-Ice"
    else "Bibata-Modern-Classic";
in {
  imports = [nix-colors.homeManagerModule];

  config = mkIf (cfg.active == "materia") {
    gtk = {
      enable = true;

      theme = {
        name = theme;
        package = pkgs.materia-theme;
      };

      iconTheme = {
        name = icons;
        package = pkgs.kora-icon-theme;
      };

      cursorTheme = {
        name = cursor;
        inherit (cfg.cursor) size;
        package = pkgs.bibata-cursors;
      };

      font = {
        inherit (cfg.font) name;
        inherit (cfg.font) size;
      };
    };

    home.pointerCursor = {
      name = cursor;
      inherit (cfg.cursor) size;
      package = pkgs.bibata-cursors;
      x11.enable = true;
      gtk.enable = true;
    };

    colorScheme = nix-colors.colorSchemes.onedark;

    xresources.properties = {"Xcursor.theme" = cursor;};
  };
}
