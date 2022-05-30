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

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = extraOverlays; #++ (lib.attrValues self.overlays);
        };

      pkgs = mkPkgs nixpkgs [ inputs.emacs-overlay.overlay ];
      
      lib = nixpkgs.lib.extend (self: super: {
        my = import ./lib {
          inherit pkgs inputs;
          lib = self;
        };
      });
      
    in {
      lib = lib.my;
      pkgs = pkgs;

      #overlays = mapModules ./overlays import;
      #packages."${system}" = mapModules ./packages (p: pkgs.callPackage p { });

      nixosModules = lib.my.mapModulesRec ./system/modules import;
      nixosConfigurations = import ./outputs/nixos.nix inputs;

      homeModules = lib.my.mapModulesRec ./home/modules import;
      homeConfigurations = import ./outputs/home.nix inputs;
    };
}
