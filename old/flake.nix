{
  description = "Personal NixOS flake configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  
  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      lib = import ./lib inputs;
 
      inherit (lib) genSystems;
      
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ inputs.emacs-overlay.overlay ];
        config.allowUnfree = true;
      };

    in {
      inherit lib pkgs;
      
      # If you want to use standalone home-manager
      inherit (import ./home/profiles inputs) homeConfigurations;

      nixosConfigurations = import ./hosts inputs;
    };
}
