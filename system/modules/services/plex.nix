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
      };

      # Plex needs read/exec permissions on files.
      # I have my drive under /home/myself/stuff, that dir has `r-x` as "others" permissions.
      users.users.plex.extraGroups = ["users"];
    }

    (mkIf (!cfg.service.enable) {systemd.services.plex.wantedBy = mkForce [];})
  ]);
}
