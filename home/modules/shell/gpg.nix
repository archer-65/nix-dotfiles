_: { config, options, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.shell.gpg;
in {
  options.user-modules.shell.gpg = with types; {
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
