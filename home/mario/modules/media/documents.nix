{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.media.documents;
  cfgTheme = config.mario.modules.themes;
in {
  options.mario.modules.media.documents = {
    zathura.enable = mkEnableOption "pdf support (zathura)";
    okular.enable = mkEnableOption "pdf support (okular)";
  };

  config = mkMerge [
    (mkIf cfg.zathura.enable {
      stylix.targets.zathura.enable = true;

      programs.zathura = {
        enable = true;
        options = {
          font =
            "${cfgTheme.font.term.family} "
            + (toString cfgTheme.font.term.size);

          recolor = "false";
          recolor-keephue = "false";
        };
      };
    })

    (mkIf cfg.okular.enable {home.packages = with pkgs; [okular];})
  ];
}
