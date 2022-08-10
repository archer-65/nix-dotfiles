{ inputs, lib, ... }:

with inputs;
with builtins;

let
  homeSet = (import ../outputs/configs.nix).homeManager.all;

  genConfiguration = home:
    { localSystem, username, ... }:
    let
      system = localSystem;

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = (lib.attrValues self.overlays) ++ [ emacs-overlay.overlay ];
      };

      baseHome = {
        inherit username;
        homeDirectory = "/home/${username}";
        stateVersion = "22.05";
      };

    in home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        { home = baseHome; }
        ("${self}/home/configurations/${home}")
        (import "${self}/mixed/options.nix" inputs)
      ] ++ attrValues self.homeModules;
    };
in { mkHome = lib.mapAttrs genConfiguration homeSet; }
