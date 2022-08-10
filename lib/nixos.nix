{ self, ... }@inputs:
let
  inherit (inputs.nixpkgs) lib;
  hosts = (import ../outputs/configs.nix).nixos.all;

  hostPkgs = { nixpkgs.pkgs = inputs.self.pkgs; };

  nixRegistry = { nix.registry = { nixpkgs.flake = inputs.nixpkgs; }; };

  genConfiguration = hostname:
    { localSystem, ... }:
    lib.nixosSystem {
      system = localSystem;
      specialArgs = { inherit lib inputs; };
      modules = [
        ("${inputs.self}/system/configurations/${hostname}")
        hostPkgs
        nixRegistry
        inputs.home-manager.nixosModules.home-manager
      ] ++ __attrValues inputs.self.nixosModules;
    };
in lib.mapAttrs genConfiguration hosts
