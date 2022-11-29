{
  config,
  lib,
  pkgs,
  nix-colors,
  ...
}:
with lib; let
  cfg = config.mario.modules.themes;

  inherit (nix-colors.lib-contrib {inherit pkgs;}) gtkThemeFromScheme;

  icons =
    if cfg.darkTheme
    then "kora"
    else "kora-light-panel";

  cursor =
    if cfg.darkTheme
    then "Bibata-Modern-Ice"
    else "Bibata-Modern-Classic";

  modus-operandi = {
    slug = "modus-operandi";
    name = "Modus Operandi";
    colors = {
      base00 = "FFFFFF"; # bg-main
      base01 = "F8F8F8"; # bg-dim
      base02 = "F0F0F0"; # bg-alt
      base03 = "EFEFEF"; # bg-inactive
      base04 = "D7D7D7"; # bg-active
      base05 = "282828"; # fg-dim
      base06 = "000000"; # fg-main
      base07 = "0A0A0A"; # fg-active
      base08 = "B60000"; # red-intense
      base09 = "904200"; # orange-intense
      base0A = "605B00"; # yellow-intense
      base0B = "006800"; # green-intense
      base0C = "005F88"; # cyan-intense
      base0D = "1F1FCE"; # blue-intense
      base0E = "7F10D0"; # purple-intense
      base0F = "A8007F"; # magenta-intense
    };
  };

  modus-vivendi = {
    slug = "modus-vivendi";
    name = "Modus Vivendi";
    colors = {
      base00 = "000000"; # bg-main
      base01 = "100F10"; # bg-dim
      base02 = "191A1B"; # bg-alt
      base03 = "1E1E1E"; # bg-inactive
      base04 = "323232"; # bg-active
      base05 = "E0E6F0"; # fg-dim
      base06 = "FFFFFF"; # fg-main
      base07 = "F4F4F4"; # fg-active
      base08 = "FE6060"; # red-intense
      base09 = "FBA849"; # orange-intense
      base0A = "F0DD60"; # yellow-intense
      base0B = "4FE42F"; # green-intense
      base0C = "3FDFD0"; # cyan-intense
      base0D = "4FAFFF"; # blue-intense
      base0E = "9F80FF"; # purple-intense
      base0F = "FF62D4"; # magenta-intense
    };
  };
in {
  imports = [nix-colors.homeManagerModule];

  config = mkIf (cfg.active == "modus") {
    gtk = {
      enable = true;

      theme = {
        name = "${config.colorscheme.slug}";
        package = gtkThemeFromScheme {scheme = config.colorscheme;};
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
        inherit (cfg.ui.font) name;
        inherit (cfg.ui.font) size;
      };

      gtk3.extraConfig = let
        val =
          if cfg.darkTheme
          then 1
          else 0;
      in {
        gtk-application-prefer-dark-theme = val;
      };
    };

    home.pointerCursor = {
      name = cursor;
      inherit (cfg.cursor) size;
      package = pkgs.bibata-cursors;
      x11.enable = true;
      gtk.enable = true;
    };

    colorScheme =
      if cfg.darkTheme
      then modus-vivendi
      else modus-operandi;

    xresources.properties = {"Xcursor.theme" = cursor;};
  };
}
