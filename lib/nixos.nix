{
  inputs,
  lib,
  flake-self,
  ...
}:
with builtins; let
  inherit (inputs) vinceliuice-grub-theme nixpkgs;
  inherit (flake-self) overlays nixosModules sharedModules;

  genConfiguration = hostname: {
    localSystem,
    stateVersion,
    ...
  }: let
    system = localSystem;

    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = lib.attrValues overlays;
    };

    baseSystem = {
      nixpkgs = {inherit pkgs;};
      system = {inherit stateVersion;};
      nix.registry = {nixpkgs.flake = nixpkgs;};
      networking.hostName = lib.mkDefault hostname;
    };
  in
    lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit lib;
        inherit flake-self;
      };
      modules =
        [
          baseSystem
          "${flake-self}/system/configurations/${hostname}"
        ]
        ++ [vinceliuice-grub-theme.nixosModule]
        ++ attrValues nixosModules
        ++ attrValues sharedModules;
    };
in
  lib.mapAttrs genConfiguration
