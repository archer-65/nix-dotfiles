{
  description = "Nix config /w home-manager and flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs@{ nixpkgs, ... }:
    let
      system = "x86_64-linux";

      lib = import ./lib inputs;

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = extraOverlays; # ++ (lib.attrValues self.overlays);
        };

      pkgs = mkPkgs nixpkgs [ inputs.emacs-overlay.overlay ];

    in {
      pkgs = pkgs;
      lib = lib;

      nixosModules = import ./system/modules inputs;
      nixosConfigurations = lib.mkSystem;

      homeModules = import ./home/modules inputs;
      homeConfigurations = lib.mkHome;
    };
}
