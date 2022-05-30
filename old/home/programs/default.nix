# This configuration files contains all the imports for the module "programs"
{ config, lib, pkgs, isDesktop, ... }:

let
  alacritty = import ./alacritty {
    inherit isDesktop pkgs;
  };
in {
  imports = [
    alacritty
    ./git
    ./shell
  ];
}