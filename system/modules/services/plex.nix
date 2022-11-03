{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.media.plex;
in {
  options.system.modules.media.plex = {
    enable = mkEnableOption "plex";
    service.enable = mkEnableOption "enable plex systemd service";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.plex = {
        enable = true;
        openFirewall = true;
        user = "${config.primaryUser.name}";
      };

      # Primary user
      primaryUser.extraGroups = ["plex"];
    }

    (mkIf (!cfg.service.enable) {systemd.services.plex.wantedBy = mkForce [];})
  ]);
}
