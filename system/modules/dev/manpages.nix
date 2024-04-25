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
      doc.enable = false;
      info.enable = true;

      nixos.enable = false;

      dev.enable = true;
      man = { 
        enable = lib.mkDefault true;
        generateCaches = lib.mkDefault true; 
      };
    };
  };
}
