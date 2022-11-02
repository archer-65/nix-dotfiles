{
  inputs,
  lib,
  ...
}:
with inputs;
with builtins; let
  homeSet = (import ../outputs/configs.nix).homeManager.all;

  genConfiguration = home: {
    localSystem,
    username,
    stateVersion,
    ...
  }: let
    system = localSystem;
    configurations = "${self}/home/configurations";

    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays =
        (lib.attrValues self.overlays)
        ++ [emacs-overlay.overlays.default];
    };

    baseHome = {
      inherit username;
      inherit stateVersion;
      homeDirectory = "/home/${username}";
    };
  in
    home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

      modules =
        [
          {home = baseHome;}
          "${configurations}/${home}"
          "${configurations}/common.nix"
          (import "${self}/mixed/options.nix" inputs)
          hyprland.homeManagerModules.default
        ]
        ++ attrValues self.homeModules;

      extraSpecialArgs = {
        inherit nix-colors;
        flake-self = self;
      };
    };
in {mkHome = lib.mapAttrs genConfiguration homeSet;}
