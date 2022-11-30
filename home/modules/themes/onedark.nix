{
  config,
  lib,
  pkgs,
  nix-colors,
  ...
}:
with lib; let
  cfg = config.mario.modules.themes;
in {
  imports = [nix-colors.homeManagerModule];

  config = mkIf (cfg.active == "onedark") {
    gtk = {
      iconTheme = let
        icons =
          if cfg.darkTheme
          then "kora"
          else "kora-light-panel";
      in {
        name = icons;
        package = pkgs.kora-icon-theme;
      };

      cursorTheme = let
        cursor =
          if cfg.darkTheme
          then "Bibata-Modern-Ice"
          else "Bibata-Modern-Classic";
      in {
        name = cursor;
        inherit (cfg.cursor) size;
        package = pkgs.bibata-cursors;
      };
    };

    colorScheme = let
      inherit (nix-colors.colorSchemes) onedark one-light;
    in
      if cfg.darkTheme
      then onedark
      else one-light;
  };
}
