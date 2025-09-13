{inputs, ...}:
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
    baseSystem = {
      nixpkgs = {
        hostPlatform = system;
        config.allowUnfree = true;
        overlays = attrValues overlays;
      };
      system = {inherit stateVersion;};
      networking.hostName = lib.mkDefault hostname;
    };
  in
    lib.nixosSystem {
      modules =
        [
          baseSystem
          "${self}/system/hosts/${hostname}"
        ]
        ++ attrValues nixosModules;

      specialArgs = {inherit inputs outputs;};
    };
in
  genConfiguration
