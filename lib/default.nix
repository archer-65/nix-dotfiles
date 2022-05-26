inputs:
let
  inherit (inputs.nixpkgs) lib;

  system = import ./system.nix inputs;
  home = import ./home.nix inputs;
  options = import ./options.nix inputs;
in lib // system // home // options
