{ config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.services.flatpak;
in {
  options.modules.services.flatpak = {
    enable = _.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.flatpak.enable = true;
  };
}