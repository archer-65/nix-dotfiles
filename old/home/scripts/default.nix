{ config, pkgs, ... }:

let
  volume = pkgs.callPackage ./volume.nix { inherit pkgs; };
in
[
  volume
]