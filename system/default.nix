{ lib, inputs, nixpkgs, home-manager, user, ... }:

let
  system = "x86_64-linux";

  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };

  lib = nixpkgs.lib;

  homeImport = import ../home/home.nix { 
    inherit pkgs;
    inherit (pkgs) config lib;
    inherit user;
    isDesktop = false;
  };

  homeDesktopImport = import ../home/home.nix { 
    inherit pkgs;
    inherit (pkgs) config lib;
    inherit user;
    isDesktop = true;
  };

in {
  ### Laptop ###
  mate = lib.nixosSystem {
    inherit system;
    specialArgs = { inherit user inputs; };
    modules = [
      ./common
      ./hosts/mate
      home-manager.nixosModules.home-manager {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.extraSpecialArgs = { inherit user; };
        home-manager.users.${user} = {
          imports = [
            homeImport
	          #../home/home.nix
	          ];
        };
      }
    ];
  };

  ### Virtual machine ###
  vm = lib.nixosSystem {
    inherit system;
    specialArgs = { inherit user inputs; };
    modules = [
      ./common
      ./hosts/vm
      home-manager.nixosModules.home-manager {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.extraSpecialArgs = { inherit user; };
        home-manager.users.${user} = {
          imports = [
            homeImport
	          #../home/home.nix
          ];
        };
      }
    ];
  };
}
