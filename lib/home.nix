{
  inputs,
  ...
}:
with builtins; let
  inherit (inputs) self nixpkgs home-manager emacs-overlay hyprland nix-colors;
  inherit (nixpkgs) lib;
  inherit (self) overlays homeModules;

  genConfiguration = {
    username,
    hostname,
    system,
    stateVersion,
    ...
  }: let
    configurations = "${self}/home/configurations";

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
          "${configurations}/${username}@${hostname}"
          "${configurations}/common.nix"
        ]
        ++ [hyprland.homeManagerModules.default]
        ++ attrValues homeModules.${username};

      extraSpecialArgs = {
        inherit nix-colors inputs;
        inherit (import ../wallpapers) wallpapers;
        flake = self;
      };
    };
in
  genConfiguration
