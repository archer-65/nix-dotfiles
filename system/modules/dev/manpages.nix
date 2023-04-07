{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.dev.manpages;
in {
  options.system.modules.dev.manpages = {
    enable = mkEnableOption "dev manpages";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [man-pages man-pages-posix];
    documentation = {
      nixos.enable = false;
      dev.enable = true;
      doc.enable = true;
      info.enable = true;
      man.generateCaches = true;
    };
  };
}
