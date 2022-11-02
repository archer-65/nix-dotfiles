{
  inputs,
  lib ? inputs.nixpkgs.lib,
  flake-self ? inputs.self,
  ...
}: {
  mkSystem = import ./nixos.nix {inherit inputs lib flake-self;};
  mkHome = import ./home.nix {inherit inputs lib flake-self;};
}
