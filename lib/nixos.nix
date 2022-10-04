{ inputs, lib, ... }:

with inputs;
with builtins;

let
  hostSet = (import ../outputs/configs.nix).nixos.all;

  genConfiguration = hostname:
    { localSystem, ... }:
    let
      system = localSystem;

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = lib.attrValues self.overlays;
      };

      baseSystem = {
        nixpkgs = { inherit pkgs; };
        nix.registry = { nixpkgs.flake = nixpkgs; };
        networking.hostName = lib.mkDefault hostname;
      };

    in lib.nixosSystem {
      inherit system;
      specialArgs = { inherit lib inputs; };
      modules = [
        baseSystem
        "${self}/system/configurations/${hostname}"
        (import "${self}/mixed/options.nix" inputs)
      ] ++ attrValues self.nixosModules;
    };

in { mkSystem = lib.mapAttrs genConfiguration hostSet; }
