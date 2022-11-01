{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.desktop.media.videos;
in {
  options.home.modules.desktop.media.videos = {
    enable = mkEnableOption "an option to watch videos";
  };

  config = mkIf cfg.enable {
    programs.mpv = {
      enable = true;
      config = {
        profile = "gpu-hq";
        hwdec = "auto";
        gpu-context = "auto";
      };
    };
  };
}
