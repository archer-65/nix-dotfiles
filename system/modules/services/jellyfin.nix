{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.media.jellyfin;
in {
  options.system.modules.media.jellyfin = {
    enable = mkEnableOption "jellyfin";
    service.enable = mkEnableOption "enable jellyfin systemd service";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.jellyfin = {
        enable = true;
        openFirewall = true;
        user = "${config.primaryUser.name}";
      };

      # Primary user
      primaryUser.extraGroups = ["jellyfin"];
    }

    (mkIf (!cfg.systemd.disable) {systemd.services.plex.wantedBy = mkForce [];})
  ]);
}
