_:
{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.core.cachix;
in {
  options.modules.core.cachix = {
    enable = mkOption {
      default = true;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.cachix ];

    nix.settings = {
      substituters = [
        "https://cache.nixos.org"
        "https://nixpkgs-wayland.cachix.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA"
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };
}
