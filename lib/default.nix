inputs: {
  mkSystem = import ./nixos.nix inputs;
  mkHome = import ./home.nix inputs;
}
