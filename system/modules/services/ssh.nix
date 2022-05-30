{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.openssh;
in {
  options.modules.services.openssh = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.openssh.enable = true;
  };
}