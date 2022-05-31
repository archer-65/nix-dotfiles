{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.media.plex;
in {
  options.modules.services.plex = {
    enable = mkBoolOpt false;
    systemd.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.plex = {
        enable = true;
        openFirewall = true;
      };
    }

    (mkIf cfg.systemd.enable == false { systemd.services.plex = mkForce { }; })

    { user.extraGroups = [ "plex" ]; }
  ]);
}
