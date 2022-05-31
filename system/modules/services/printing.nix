{ config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.services.printing;
in {
  options.modules.services.printing = {
    enable = _.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # Enable CUPS
    services.printing = {
      enable = true;
      drivers = [ pkgs.hplip ];
    };
  };
}