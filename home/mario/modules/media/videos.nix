{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.media.videos;
in {
  options.mario.modules.media.videos = {
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
