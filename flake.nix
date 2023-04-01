{
  description = "Nix config /w home-manager and flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-colors.url = "github:misterio77/nix-colors";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/Hyprland";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    notmuch-mailmover.url = "github:michaeladler/notmuch-mailmover";
  };

  outputs = inputs @ {nixpkgs, ...}: let
    lib = import ./lib {inherit inputs;};
    inherit (lib) mkSystem mkHome forAllSystems;
  in {
    nixosModules = import ./system/modules;
    homeModules = import ./home/modules;

    overlays = import ./overlays inputs;

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
  };
}
