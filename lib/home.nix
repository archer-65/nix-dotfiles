{
  inputs,
  lib,
  flake-self,
  ...
}:
with builtins; let
  inherit (inputs) nixpkgs home-manager emacs-overlay hyprland nix-colors;
  inherit (flake-self) overlays homeModules sharedModules;

  genConfiguration = home: {
    localSystem,
    username,
    stateVersion,
    ...
  }: let
    system = localSystem;
    configurations = "${flake-self}/home/configurations";

    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays =
        (lib.attrValues overlays)
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
        ]
        ++ [hyprland.homeManagerModules.default]
        ++ attrValues homeModules.${username}
        ++ attrValues sharedModules;

      extraSpecialArgs = {
        inherit nix-colors flake-self inputs;
      };
    };
in
  homeSet: lib.mapAttrs genConfiguration homeSet
