{ inputs, lib, ... }:

with lib;
with lib.my;
let
  #inherit (inputs.nixpkgs) lib;
  #inherit (lib) lib;
  #inherit (lib.my) mapModules mapModulesRec mapModules' mapModulesRec' mkHost;
  hosts = (import ./configs.nix).nixos.all;

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
      modules = [
        ("${inputs.self}/system/configurations/${hostname}")
        netHostMap
        hostPkgs
        nixRegistry
        inputs.home-manager.nixosModules.home-manager
      ] 
      ++ (mapModulesRec' (toString ../system/modules) import);
      #++ __attrValues inputs.self.nixosModules;
    };
in
lib.mapAttrs genConfiguration hosts