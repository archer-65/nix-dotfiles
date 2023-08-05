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
      };

      # Jellyfin needs read/exec permissions on files.
      # I have my drive under /home/myself/stuff, that dir has `r-x` as "others" permissions.
      users.users.jellyfin.extraGroups = ["users"];
    }

    (mkIf (!cfg.service.enable) {systemd.services.jellyfin.wantedBy = mkForce [];})
  ]);
}
