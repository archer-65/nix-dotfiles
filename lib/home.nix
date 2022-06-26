{ self, ... }@inputs:
let
  inherit (inputs.nixpkgs) lib;
  homeSet = (import ../outputs/configs.nix).homeManager.all;

  genConfiguration = home:
    { localSystem, username, ... }:
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = inputs.self.pkgs;
      modules = [{
        home = {
          inherit username;
          homeDirectory = "/home/${username}";
          stateVersion = "22.05";
        };
      }] ++ [ (import "${inputs.self}/home/configurations/${home}") ]
        ++ [ (import "${inputs.self}/mixed/options.nix" inputs) ]
        ++ __attrValues inputs.self.homeModules;
      # system = localSystem;
    };
in lib.mapAttrs genConfiguration homeSet
