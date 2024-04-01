{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.mario.modules.themes;
in {
  imports = [inputs.nix-colors.homeManagerModule];

  config = mkIf (cfg.active == "modus") {
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
      modus-operandi = {
        slug = "modus-operandi";
        name = "Modus Operandi";
        palette = {
          base00 = "#FFFFFF"; # bg-main
          base01 = "#F0F0F0"; # bg-alt
          base02 = "#D7D7D7"; # bg-active
          base03 = "#505050"; # fg-alt
          base04 = "#404148"; # fg-active
          base05 = "#282828"; # fg-dim
          base06 = "#000000"; # fg-main
          base07 = "#0A0A0A"; # fg-active
          base08 = "#B60000"; # red-intense
          base09 = "#904200"; # orange-intense
          base0A = "#605B00"; # yellow-intense
          base0B = "#006800"; # green-intense
          base0C = "#005F88"; # cyan-intense
          base0D = "#1F1FCE"; # blue-intense
          base0E = "#7F10D0"; # purple-intense
          base0F = "#A8007F"; # magenta-intense
        };
      };

      modus-vivendi = {
        slug = "modus-vivendi";
        name = "Modus Vivendi";
        palette = {
          base00 = "#000000"; # bg-main
          base01 = "#191A1B"; # bg-alt
          base02 = "#323232"; # bg-active
          base03 = "#A8A8A8"; # fg-alt
          base04 = "#BFC0C4"; # fg-inactive
          base05 = "#E0E6F0"; # fg-dim
          base06 = "#FFFFFF"; # fg-main
          base07 = "#F4F4F4"; # fg-active
          base08 = "#FE6060"; # red-intense
          base09 = "#FBA849"; # orange-intense
          base0A = "#F0DD60"; # yellow-intense
          base0B = "#4FE42F"; # green-intense
          base0C = "#3FDFD0"; # cyan-intense
          base0D = "#4FAFFF"; # blue-intense
          base0E = "#9F80FF"; # purple-intense
          base0F = "#FF62D4"; # magenta-intense
        };
      };
    in
      if cfg.darkTheme
      then modus-vivendi
      else modus-operandi;
  };
}
