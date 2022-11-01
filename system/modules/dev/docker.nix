{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.dev.docker;
in {
  options.modules.dev.docker = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    virtualisation.docker = { enable = true; };

    user.extraGroups = [ "docker" ];
  };
}
