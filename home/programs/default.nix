# This configuration files contains all the imports for the module "programs"
{ config, lib, pkgs, inputs, user, ... }:
{
  imports = [
    ./alacritty
    ./git
  ];
}