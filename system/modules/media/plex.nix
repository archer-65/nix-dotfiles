{ options, config, lib, pkgs, ... }:

with lib;
#with lib.my;
let cfg = config.modules.media.plex;
in {
  options.modules.media.plex = {
    enable = _.mkBoolOpt false;
    systemd.disable = _.mkBoolOpt true;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.plex = {
        enable = true;
        openFirewall = true;
      };
    }

    (mkIf cfg.systemd.enable { systemd.services.plex = mkForce { }; })

    { user.extraGroups = [ "plex" ]; }
  ]);
}
