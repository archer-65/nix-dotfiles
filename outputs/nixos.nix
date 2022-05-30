{self, nixpkgs, home-manager, ...}:
let
  inherit (self.pkgs) lib;
  hosts = (import ./configs.nix).nixos.all;

  netHostMap = {
    networking.hosts = lib.mapAttrs' (n: v: lib.nameValuePair v.address [ n ]) hosts;
  };

  hostPkgs = {
    nixpkgs.pkgs = self.pkgs;
  };

  genConfiguration = hostname: { localSystem, ... }:
    lib.nixosSystem {
      system = localSystem;
      modules = [
        ("${self}/system/configurations/${hostname}")
        netHostMap
        hostPkgs
        home-manager.nixosModules.home-manager
      ]
      ++ __attrValues self.nixosModules;
    };
in
lib.mapAttrs genConfiguration hosts