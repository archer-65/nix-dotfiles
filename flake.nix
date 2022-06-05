{
  description = "Nix config /w home-manager and flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    #emacs-overlay.url = "https://github.com/nix-community/emacs-overlay/archive/master@{2%20hours%20ago}.tar.gz";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      system = "x86_64-linux";

      lib = nixpkgs.lib;
      helpers = import ./lib inputs;

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = extraOverlays ++ (lib.attrValues self.overlays);
        };

      pkgs = mkPkgs nixpkgs [ inputs.emacs-overlay.overlay ];

    in {
      pkgs = pkgs;

      # Expose overlay to flake outputs, to allow using it from other flakes.
      # Flake inputs are passed to the overlay so that the packages defined in
      # it can use the sources pinned in flake.lock
      overlays.default = final: prev: (import ./overlays inputs) final prev;

      nixosModules = import ./system/modules inputs;
      nixosConfigurations = helpers.mkSystem;

      homeModules = import ./home/modules inputs;
      homeConfigurations = helpers.mkHome;
    };
}
