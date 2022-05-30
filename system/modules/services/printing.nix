{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.printing;
in {
  options.modules.services.printing = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # Enable CUPS
    printing = {
      enable = true;
      drivers = [ pkgs.hplip ];
    };
  };
}