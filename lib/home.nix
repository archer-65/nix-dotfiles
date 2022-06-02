{ self, ... }@inputs:
let
  inherit (inputs.nixpkgs) lib;
  homeSet = (import ../outputs/configs.nix).homeManager.all;

  genModules = home:
    { lib, ... }: {
      imports = [ (import "${inputs.self}/home/configurations/${home}") ]
        ++ __attrValues inputs.self.homeModules;
    };

  genConfiguration = home:
    { homeDirectory, localSystem, username, ... }:
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit username;
      homeDirectory = "/home/${username}";
      configuration = genModules home;
      pkgs = inputs.self.pkgs;
      stateVersion = "22.05";
      system = localSystem;
    };
in lib.mapAttrs genConfiguration homeSet
