{ lib, inputs, nixpkgs, home-manager, user, ... }:

let
  system = "x86_64-linux";

  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };

  lib = nixpkgs.lib;

  termAlternative = import ../home/programs/alacritty { fontSize = 10.0; inherit pkgs; };
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
	    ../home/home.nix
	    termAlternative
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
	    import ../home/home.nix
	    termAlternative
	  ];
        };
      }
    ];
  };
}
