{ config, pkgs, ... }:

let
  usedcpu = pkgs.callPackage ./usedcpu.nix { inherit pkgs; };
  usedram = pkgs.callPackage ./usedram.nix { inherit pkgs; };
in [ usedcpu usedram ]
