{
  inputs,
  ...
}:
with builtins; let
  inherit (inputs) self nixpkgs;
  inherit (nixpkgs) lib;
  inherit (self) outputs overlays nixosModules;

  genConfiguration = {
    hostname,
    system,
    stateVersion,
    ...
  }: let
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
        # inherit lib;
        inherit (import ../wallpapers) wallpapers;
        flake = self;
        homeConfig = outputs.homeConfigurations."mario@${hostname}".config;
      };
      modules =
        [
          baseSystem
          "${self}/system/configurations/${hostname}"
        ]
        ++ attrValues nixosModules;
    };
in
  genConfiguration
