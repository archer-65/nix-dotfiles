{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.media.plex;
in {
  options.modules.media.plex = {
    enable = mkEnableOption "plex";
    service.enable = mkEnableOption "enable plex systemd service";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.plex = {
        enable = true;
        openFirewall = true;
        user = "${config.user.name}";
      };

      # Primary user
      user.extraGroups = ["plex"];
    }

    (mkIf (!cfg.service.enable) {systemd.services.plex.wantedBy = mkForce [];})
  ]);
}
