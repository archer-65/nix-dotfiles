{
  description = "Nix config /w home-manager and flakes";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.05";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # This for direct use of the overlay
    # emacs-overlay.url = "github:nix-community/emacs-overlay?rev=977b205ab9ce857f3440dff2a114a35bf2758c05";

    # This to follow another nixpkgs input
    emacs-overlay = {
      url = "github:jeslie0/emacs-overlay";
      inputs.nixpkgs.url = "github:nixos/nixpkgs/a2a777538d971c6b01c6e54af89ddd6567c055e8";
      inputs.emacs-overlay.url = "github:nix-community/emacs-overlay?rev=307dfb67a8080125c50d0c99f6bf6178a23395f7";
    };

    vinceliuice-grub-theme = {
      url = "github:vinceliuice/grub2-themes";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/Hyprland";

    nix-colors.url = "github:misterio77/nix-colors";

    webcord.url = "github:fufexan/webcord-flake";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    ...
  }: let
    lib = import ./lib {inherit inputs;};

    hosts = (import ./outputs/configs.nix nixpkgs.lib).nixos;
    homes = (import ./outputs/configs.nix nixpkgs.lib).home-manager;
  in {
    nixosModules = import ./system/modules;
    homeModules = import ./home/modules;
    sharedModules = import ./shared;

    nixosConfigurations = lib.mkSystem hosts;
    homeConfigurations = lib.mkHome homes;

    overlays = import ./overlays inputs;

    formatter = lib.forAllSystems (
      system:
        nixpkgs.legacyPackages.${system}.alejandra
    );

    packages = lib.forAllSystems (
      system:
        import ./packages {pkgs = nixpkgs.legacyPackages.${system};}
    );

    devShell =
      lib.forAllSystems (system:
        import ./shell.nix {pkgs = nixpkgs.legacyPackages.${system};});
  };
}
