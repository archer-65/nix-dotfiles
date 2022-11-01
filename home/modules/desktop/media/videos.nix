{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.desktop.media.videos;
in {
  options.user-modules.desktop.media.videos = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
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
