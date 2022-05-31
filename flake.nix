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

      lib = nixpkgs.lib.extend (self: super: {
        my = import ./lib {
          inherit nixpkgs inputs;
          lib = self;
        };
      });

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = extraOverlays; #++ (lib.attrValues self.overlays);
        };

      pkgs = mkPkgs nixpkgs [ inputs.emacs-overlay.overlay ];

      inherit (lib.my) mapModules mapModulesRec mapModules' mapModulesRec' mkHost;
    in {
      pkgs = pkgs;
      lib = lib.my;

      #overlays = mapModules ./overlays import;
      #packages."${system}" = mapModules ./packages (p: pkgs.callPackage p { });

      nixosModules = mapModules ./system/modules import;
      nixosConfigurations = import ./outputs/nixos.nix inputs;

      homeModules = mapModules ./home/modules import;
      homeConfigurations = import ./outputs/home.nix inputs;
    };
}
