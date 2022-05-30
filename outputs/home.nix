{self, nixpkgs, home-manager, ...}:
let
  inherit (self.pkgs) lib;
  homeSet = (import ./configs.nix).homeManager.all;

  genModules = home:
    { config, ... }: {
      imports = [ ("${self}/system/configurations/${home}") ]
      ++ __attrValues self.homeModules;
    };

  genConfiguration = home: {homeDirectory, localSystem, username, ... }:
    home-manager.lib.homeManagerConfiguration {
      inherit homeDirectory username;
      configuration = genModules home;
      pkgs = self.pkgs;
      stateVersion = "22.05";
      system = localSystem;
    };
in
lib.mapAttrs genConfiguration homeSet