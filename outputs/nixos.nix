inputs:
let
  inherit (inputs.self.pkgs) lib;
  hosts = (import ./configs.nix).nixos.all;

  netHostMap = {
    networking.hosts = lib.mapAttrs' (n: v: lib.nameValuePair v.address [ n ]) hosts;
  };

  hostPkgs = {
    nixpkgs.pkgs = inputs.self.pkgs;
  };

  genConfiguration = hostname: { localSystem, ... }:
    lib.nixosSystem {
      system = localSystem;
      modules = [
        ("${inputs.self}/system/configurations/${hostname}")
        netHostMap
        hostPkgs
        inputs.home-manager.nixosModules.home-manager
      ]
      ++ __attrValues inputs.self.nixosModules;
    };
in
lib.mapAttrs genConfiguration hosts