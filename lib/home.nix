{ inputs, lib, ... }:
let
  inherit lib;
  homeSet = (import ../outputs/configs.nix).homeManager.all;

  genModules = home:
    { config, lib, ... }: {
      imports = 
      [ ("${inputs.self}/system/configurations/${home}") ]
      ++ (lib._.mapModulesRec' (toString ../home/modules) import);
      #++ __attrValues inputs.self.homeModules;
    };

  genConfiguration = home: {homeDirectory, localSystem, username, ... }:
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit homeDirectory username;
      configuration = genModules home;
      pkgs = inputs.self.pkgs;
      stateVersion = "22.05";
      system = localSystem;
    };
in {  
  generateHomes = lib.mapAttrs genConfiguration homeSet;
}