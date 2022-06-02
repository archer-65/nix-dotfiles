_:
{ config, options, lib, ... }:

with lib;
let cfg = config.modules.services.ssh;
in {
  options.modules.services.ssh = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      startWhenNeeded = true;
      openFirewall = true;
    };

    services.sshd.enable = true;
  };
}
