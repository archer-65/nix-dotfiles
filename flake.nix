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

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";

      # lib = nixpkgs.lib.extend (self: super: {
      #   my = import ./lib {
      #     inherit nixpkgs inputs;
      #     lib = self;
      #   };
      # });
      lib = import ./lib { inherit pkgs inputs; lib = nixpkgs.lib; };

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = extraOverlays; #++ (lib.attrValues self.overlays);
        };

      pkgs = mkPkgs nixpkgs [ inputs.emacs-overlay.overlay ];

      inherit (lib._) mapModules mapModulesRec mapModules' mapModulesRec' generateHosts;
    in {
      pkgs = pkgs;
      # lib = lib;

      #overlays = mapModules ./overlays import;
      #packages."${system}" = mapModules ./packages (p: pkgs.callPackage p { });

      nixosModules = mapModulesRec ./system/modules import;
      #nixosConfigurations = import ./outputs/nixos.nix {inherit inputs; inherit lib;};
      nixosConfigurations = generateHosts; 

      homeModules = mapModulesRec ./home/modules import;
      homeConfigurations = import ./outputs/home.nix {inherit inputs; inherit lib;};
    };
}
