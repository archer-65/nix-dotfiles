{ inputs, lib, ... }:

with inputs;
with builtins;

let
  hostSet = (import ../outputs/configs.nix).nixos.all;

  genConfiguration = hostname:
    { localSystem, stateVersion, ... }:
    let
      system = localSystem;

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = lib.attrValues self.overlays;
      };

      baseSystem = {
        nixpkgs = { inherit pkgs; };
        system = { inherit stateVersion; };
        nix.registry = { nixpkgs.flake = nixpkgs; };
        networking.hostName = lib.mkDefault hostname;
      };

    in lib.nixosSystem {
      inherit system;
      specialArgs = { inherit lib; flake-self = self; };
      modules = [
        baseSystem
        vinceliuice-grub-theme.nixosModule
        "${self}/system/configurations/${hostname}"
        (import "${self}/mixed/options.nix" inputs)
      ] ++ attrValues self.nixosModules;
    };

in { mkSystem = lib.mapAttrs genConfiguration hostSet; }
