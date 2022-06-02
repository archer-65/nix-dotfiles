_: { config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.gnupg;
in {
  options.modules.shell.gnupg = with types; {
    enable   = mkBoolOpt false;
    #cacheTTL = mkOpt int 3600;  # 1hr
  };

  config = mkIf cfg.enable {
    programs.gnupg.agent = {
      enable = true;
      pinentryFlavor = "gtk3";
      enableSSHSupport = true;
    };
  };
}