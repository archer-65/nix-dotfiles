{
  description = "Nix config /w home-manager and flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };

    mac-app-util = {
      url = "github:hraban/mac-app-util";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };

    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland = {
      # Set a tag when you want to pin Hyprland
      # url = "github:hyprwm/Hyprland/v0.46.2";
      url = "github:hyprwm/Hyprland";
    };

    sops-nix.url = "github:Mic92/sops-nix";
    nix-colors.url = "github:misterio77/nix-colors";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixpkgs-darwin,
    home-manager,
    darwin,
    ...
  }: let
    inherit (self) outputs;
    lib = import ./lib {inherit inputs;};
    inherit (lib) mkSystem mkHome mkDarwin forAllSystems;
  in {
    nixosModules = import ./system/modules;
    homeModules = import ./home/mario/modules;

    overlays = import ./overlays {inherit inputs outputs;};

    formatter = forAllSystems (
      system:
        nixpkgs.legacyPackages.${system}.alejandra
    );

    packages = forAllSystems (
      system:
        import ./packages {pkgs = nixpkgs.legacyPackages.${system};}
    );

    devShells = forAllSystems (
      system:
        import ./shell.nix {pkgs = nixpkgs.legacyPackages.${system};}
    );

    wallpapers = import ./wallpapers;

    nixosConfigurations = {
      quietfrost = mkSystem {
        hostname = "quietfrost";
        system = "x86_64-linux";
        stateVersion = "22.05";
      };
      mate = mkSystem {
        hostname = "mate";
        system = "x86_64-linux";
        stateVersion = "22.05";
      };
    };

    homeConfigurations = {
      "mario@quietfrost" = mkHome {
        username = "mario";
        hostname = "quietfrost";
        system = "x86_64-linux";
        stateVersion = "22.05";
      };
      "mario@mate" = mkHome {
        username = "mario";
        hostname = "mate";
        system = "x86_64-linux";
        stateVersion = "22.05";
      };
    };

    darwinConfigurations = {
      macbook = mkDarwin {
        hostname = "macbook";
        username = "m.liguori";
        system = "aarch64-darwin";
        stateVersion = 5;
        homeStateVersion = "25.05";
      };
    };
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nixpkgs-wayland.cachix.org"
      "https://nix-community.cachix.org"
      "https://hyprland.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
    ];
  };
}
