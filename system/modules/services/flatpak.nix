_: { config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.services.flatpak;
in {
  options.modules.services.flatpak = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    services.flatpak.enable = true;
  };
}