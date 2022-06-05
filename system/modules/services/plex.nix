_:
{ options, config, lib, ... }:

with lib;

let cfg = config.modules.media.plex;
in {
  options.modules.media.plex = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };

    systemd.disable = mkOption {
      default = true;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.plex = {
        enable = true;
        openFirewall = true;
      };
    }

    (mkIf cfg.systemd.disable { systemd.services.plex.wantedBy = mkForce []; })

    { user.extraGroups = [ "plex" ]; }
  ]);
}
