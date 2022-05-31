inputs:
let
  inherit (inputs.nixpkgs) lib;
  homeSet = (import ./configs.nix).homeManager.all;

  genModules = home:
    { config, ... }: {
      imports = [ ("${inputs.self}/system/configurations/${home}") ]
      ++ __attrValues inputs.self.homeModules;
    };

  genConfiguration = home: {homeDirectory, localSystem, username, ... }:
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit homeDirectory username;
      configuration = genModules home;
      pkgs = inputs.self.pkgs;
      stateVersion = "22.05";
      system = localSystem;
    };
in
lib.mapAttrs genConfiguration homeSet