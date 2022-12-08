{
  description = "Nix config /w home-manager and flakes";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # emacs-overlay = {
    #   url = "github:jeslie0/emacs-overlay";
    #   inputs.nixpkgs.url = "github:nixos/nixpkgs/e76c78d20685a043d23f5f9e0ccd2203997f1fb1";
    #   inputs.emacs-overlay.url = "github:nix-community/emacs-overlay?rev=e88e8c7f0c77622bb3704ea38f146a6e353445b6";
    # };

    emacs-overlay.url = "github:nix-community/emacs-overlay?rev=49e3c66d211d5110909375fe48d85c3c43753d61";

    vinceliuice-grub-theme = {
      url = "github:vinceliuice/grub2-themes";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/Hyprland";

    nix-colors.url = "github:misterio77/nix-colors";

    notmuch-mailmover.url = "github:michaeladler/notmuch-mailmover";
  };

  outputs = inputs @ {
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

    devShells = lib.forAllSystems (
      system: {
        default = import ./shell.nix {pkgs = nixpkgs.legacyPackages.${system};};
      }
    );
  };
}
