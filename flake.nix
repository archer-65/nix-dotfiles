{
  description = "Nix config /w home-manager and flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";

    emacs-overlay.url =
      "github:nix-community/emacs-overlay?rev=9f50a8d3be2291db155cb751448eb2e34d12b5e4";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      lib = import ./lib { inherit inputs; };
    in {
      # Expose overlay to flake outputs, to allow using it from other flakes.
      # Flake inputs are passed to the overlay so that the packages defined in
      # it can use the sources pinned in flake.lock
      overlays.default = final: prev: (import ./overlays inputs) final prev;

      nixosModules = import ./system/modules inputs;
      nixosConfigurations = lib.mkSystem;

      homeModules = import ./home/modules inputs;
      homeConfigurations = lib.mkHome;
    };
}
