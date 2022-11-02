{
  description = "Nix config /w home-manager and flakes";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    utils.url = "github:gytis-ivaskevicius/flake-utils-plus";

    # This for direct use of the overlay
    # emacs-overlay.url = "github:nix-community/emacs-overlay?rev=977b205ab9ce857f3440dff2a114a35bf2758c05";

    # This to follow another nixpkgs input
    emacs-overlay = {
      url = "github:jeslie0/emacs-overlay";
      inputs.nixpkgs.url = "github:nixos/nixpkgs/fdebb81f45a1ba2c4afca5fd9f526e1653ad0949";
      inputs.emacs-overlay.url = "github:nix-community/emacs-overlay?rev=4a44c7dfdea3e794b25eae37773c9a89c4fb1526";
    };

    vinceliuice-grub-theme = {
      url = "github:vinceliuice/grub2-themes";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/Hyprland";

    nix-colors.url = "github:misterio77/nix-colors";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    utils,
    ...
  }: let
    lib = import ./lib {inherit inputs;};

    hosts = (import ./outputs/configs.nix nixpkgs.lib).nixos;
    homes = (import ./outputs/configs.nix nixpkgs.lib).home-manager;

    inherit (utils.lib) eachDefaultSystem;
  in
    {
      nixosModules = import ./system/modules {inherit utils;};
      homeModules = import ./home/modules {inherit utils;};
      sharedModules = import ./shared {inherit utils;};

      nixosConfigurations = lib.mkSystem hosts;
      homeConfigurations = lib.mkHome homes;

      overlays = import ./overlays inputs;
    }
    // eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        # Generate formatter (alejandra) attribute set for all systems
        formatter = pkgs.alejandra;

        packages = import ./packages {inherit pkgs;};

        devShell = import ./shell.nix {inherit pkgs;};
      }
    );
}
