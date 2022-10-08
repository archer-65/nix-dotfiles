# modules/hardware/razer.nix --- support for razer devices
_:
{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.hardware.razer;
in {
  options.modules.hardware.razer = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    hardware.openrazer.enable = true;

    user.extraGroups = [ "openrazer" ];

    environment.systemPackages = with pkgs; [ polychromatic ];
  };
}
