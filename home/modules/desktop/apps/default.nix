# This configuration files contains all the imports for the module "programs"
{ config, lib, pkgs, ... }:

{ imports = [ ./alacritty ./git ./shell ./rofi ./greenclip ]; }