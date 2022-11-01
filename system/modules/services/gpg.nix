{ config, options, lib, ... }:

with lib;
let cfg = config.modules.services.gpg;
in {
  options.modules.services.gpg = {
    enable = mkEnableOption "gpg configuration";
  };

  config = mkIf cfg.enable {
    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # programs.mtr.enable = true;
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gnome3";
    };
  };
}
