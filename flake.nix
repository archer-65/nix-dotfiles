{
  description = "Personal NixOS flake configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let user = "mario";
    in {

      nixosConfigurations = (import ./system {
        inherit (nixpkgs) lib;
        inherit inputs user nixpkgs home-manager;
      });
    };
}
