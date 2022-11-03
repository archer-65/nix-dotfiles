{
  inputs,
  lib ? inputs.nixpkgs.lib,
  flake-self ? inputs.self,
  ...
}: let
  supportedSystems = ["x86_64-linux" "aarch64-linux"];
in {
  mkSystem = import ./nixos.nix {inherit inputs lib flake-self;};
  mkHome = import ./home.nix {inherit inputs lib flake-self;};

  forAllSystems = lib.genAttrs supportedSystems;
}
