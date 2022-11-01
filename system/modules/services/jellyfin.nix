{ options, config, lib, ... }:

with lib;

let cfg = config.modules.media.jellyfin;
in {
  options.modules.media.jellyfin = {
    enable = mkEnableOption "jellyfin";
    service.enable = mkEnableOption "enable jellyfin systemd service";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.jellyfin = {
        enable = true;
        openFirewall = true;
        user = "${config.user.name}";
      };

      # Primary user
      user.extraGroups = [ "jellyfin" ];
    }

    (mkIf (!cfg.systemd.disable) { systemd.services.plex.wantedBy = mkForce [ ]; })
  ]);
}
