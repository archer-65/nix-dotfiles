{ inputs, lib, ... }:
let
  inherit lib;
  homeSet = (import ../outputs/configs.nix).homeManager.all;
  modules = (lib._.mapModulesRec' (toString ../home/modules) import);

  genModules = home: { lib, ... }: {
    lib = { added = import ./options.nix; };
    imports = [
      (import "${inputs.self}/home/configurations/${home}") 
    ] 
    ++
    modules;
    #(lib._.mapModulesRec' (toString ../home/modules) import);
    #++ __attrValues inputs.self.homeModules;
  };

  genConfiguration = home: {homeDirectory, localSystem, username, ... }:
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit homeDirectory username;
      configuration = genModules home { inherit lib; };
      pkgs = inputs.self.pkgs;
      stateVersion = "22.05";
      system = localSystem;
    };
in {  
  generateHomes = lib.mapAttrs genConfiguration homeSet;
}