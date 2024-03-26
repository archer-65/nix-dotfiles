{
  description = "Nix config /w home-manager and flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Remove this after awscli2 >= 2.15.31 reaches nixos-unstable
    nixpkgs-awscli-2-15-31.url = "github:nixos/nixpkgs/c726225724e681b3626acc941c6f95d2b0602087";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/Hyprland";

    sops-nix.url = "github:Mic92/sops-nix";
    nix-colors.url = "github:misterio77/nix-colors";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    ...
  }: let
    inherit (self) outputs;
    lib = import ./lib {inherit inputs;};
    inherit (lib) mkSystem mkHome forAllSystems;
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
      mli-pc = mkSystem {
        hostname = "mli-pc";
        system = "x86_64-linux";
        stateVersion = "23.05";
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
      "mario@mli-pc" = mkHome {
        username = "mario";
        hostname = "mli-pc";
        system = "x86_64-linux";
        stateVersion = "23.05";
      };
    };
  };
}
