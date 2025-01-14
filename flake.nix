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
      url = "github:hyprwm/Hyprland/v0.45.2";
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

    darwinConfigurations = let
      system = "aarch64-darwin";
      pkgs = import nixpkgs-darwin {
        inherit system;
        config.allowUnfree = true;
        overlays = builtins.attrValues self.outputs.overlays;
      };
    in {
      macbook = darwin.lib.darwinSystem {
        inherit system;
        inherit pkgs;
        modules = [
          {
            system.stateVersion = 5;
            users.users."m.liguori" = {
              name = "m.liguori";
              home = "/Users/m.liguori";
            };

            nix.settings.trusted-users = ["m.liguori"];

            homebrew = {
              enable = true;

              onActivation = {
                cleanup = "uninstall";
              };

              casks = [
                "firefox"
                "docker"
                "microsoft-teams"
                "microsoft-excel"
                "tunnelblick"
                "keybase"
                "karabiner-elements"
              ];
            };
          }

          inputs.mac-app-util.darwinModules.default

          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {inherit inputs outputs;};
            home-manager.sharedModules = [inputs.mac-app-util.homeManagerModules.default];
            home-manager.users."m.liguori" = {
              imports = (builtins.attrValues self.outputs.homeModules.mario) ++ [./home/m.liguori/hosts/macbook.nix];

              home = {
                username = "m.liguori";
                stateVersion = "25.05";
                homeDirectory = "/Users/m.liguori";
              };
            };
          }
        ];
        specialArgs = {inherit inputs outputs;};
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
