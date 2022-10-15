{ inputs, lib, ... }:

with inputs;
with builtins;

let
  homeSet = (import ../outputs/configs.nix).homeManager.all;

  genConfiguration = home:
    { localSystem, username, stateVersion, ... }:
    let
      system = localSystem;

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = (lib.attrValues self.overlays) ++ [ emacs-overlay.overlay ];
      };

      baseHome = {
        inherit username;
        inherit stateVersion;
        homeDirectory = "/home/${username}";
      };

    in home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        { home = baseHome; }
        "${self}/home/configurations/${home}"
        (import "${self}/mixed/options.nix" inputs)
        hyprland.homeManagerModules.default
      ] ++ attrValues self.homeModules;
      extraSpecialArgs = { inherit nix-colors; };
    };
in { mkHome = lib.mapAttrs genConfiguration homeSet; }
