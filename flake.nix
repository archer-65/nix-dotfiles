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
      "github:nix-community/emacs-overlay?rev=f54f1ef6a85f892b57c2d020d17afe52f163a651";
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
