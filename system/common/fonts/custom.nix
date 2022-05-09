{ pkgs, ... }:
{
  feather = pkgs.callPackage ./feather.nix { };
}