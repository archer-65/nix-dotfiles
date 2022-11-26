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
    emacs-overlay.url = "github:nix-community/emacs-overlay?rev=4b6569a054e693a9a7d6eef423fac9b506961b76";

    # This to follow another nixpkgs input
    # emacs-overlay = {
    #   url = "github:jeslie0/emacs-overlay";
    #   inputs.nixpkgs.url = "github:nixos/nixpkgs/52b2ac8ae18bbad4374ff0dd5aeee0fdf1aea739";
    #   inputs.emacs-overlay.url = "github:nix-community/emacs-overlay?rev=909b090c1181644ef3def6a37a18e9e3d08d1b07";
    # };

    vinceliuice-grub-theme = {
      url = "github:vinceliuice/grub2-themes";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/Hyprland";

    nix-colors.url = "github:misterio77/nix-colors";

    webcord.url = "github:fufexan/webcord-flake";

    notmuch-mailmover.url = "github:michaeladler/notmuch-mailmover";
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
