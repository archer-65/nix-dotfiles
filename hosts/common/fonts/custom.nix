{ pkgs, ... }:
{
  feather = pkgs.callPackage ./Feather { };
  comfortaa-rofi = pkgs.callPackage ./Comfortaa { };
}