# modules/hardware/razer.nix --- support for razer devices
{ options, config, lib, pkgs, ... }:

with lib;
;
let cfg = config.modules.hardware.razer;
in {
  options.modules.hardware.razer = {
    enable = _.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    hardware.openrazer.enable = true;

    user.extraGroups = [ "plugdev" ];

    environment.systemPackages = with pkgs; [
      razergenie
    ];
  };
}