{ options, config, lib, pkgs, ... }: 

with lib;
;
let cfg = config.modules.core.cachix;
in {
  options.modules.core.cachix = {
    enable = _.mkBoolOpt true;
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