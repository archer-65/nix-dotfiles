_:
{ config, options, lib, ... }:

with lib;
let cfg = config.user-modules.credentials.gpg;
in {
  options.user-modules.credentials.gpg = with types; {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
    #cacheTTL = mkOpt int 3600;   # 1hr
  };

  config = mkIf cfg.enable {
    services.gpg-agent = {
      enable = true;
      pinentryFlavor = "gnome3";
      enableSshSupport = true;
    };
  };
}
