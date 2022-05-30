{ options, config, lib, pkgs, ... }: 

with lib;
with lib.my;
let cfg = config.modules.cachix;
in {
  options.modules.cachix = {
    enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable { 
    environment.systemPackages = [ pkgs.cachix ];

    nix = {
      binaryCaches = [
        "https://nix-community.cachix.org"
      ];
      binaryCachePublicKeys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };
}