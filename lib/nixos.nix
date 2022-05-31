{ inputs, lib, ... }:
let
  inherit lib;
  hosts = (import ../outputs/configs.nix).nixos.all;

  netHostMap = {
    networking.hosts = lib.mapAttrs' (n: v: lib.nameValuePair v.address [ n ]) hosts;
  };

  hostPkgs = {
    nixpkgs.pkgs = inputs.self.pkgs;
  };

  nixRegistry = {
    nix.registry = {
      nixpkgs.flake = inputs.nixpkgs;
    };
  };

  genConfiguration = hostname: { localSystem, ... }:
    lib.nixosSystem {
      system = localSystem;
      specialArgs = { inherit lib inputs; };
      modules = [
        ("${inputs.self}/system/configurations/${hostname}")
        netHostMap
        hostPkgs
        nixRegistry
        inputs.home-manager.nixosModules.home-manager
      ]
      ++ (lib._.mapModulesRec' (toString ../system/modules) import);
      #++ __attrValues inputs.self.nixosModules;
    };
in {
  generateHosts = lib.mapAttrs genConfiguration hosts;
}