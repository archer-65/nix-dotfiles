_: { config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.services.printing;
in {
  options.modules.services.printing = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    # Enable CUPS
    services.printing = {
      enable = true;
      drivers = [ pkgs.hplip ];
    };
  };
}