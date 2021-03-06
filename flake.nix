{
  description = "Nix config /w home-manager and flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay?rev=4d03024af95e8338ccd4d238a46c4bbe01ecdb89";
  };

  outputs = inputs@{ self, nixpkgs, nur, ... }:
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

      pkgs = mkPkgs nixpkgs [
        inputs.emacs-overlay.overlay
        inputs.nur.overlay
      ];

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
