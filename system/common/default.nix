# This configuration files contain common settings (of the system)
# between all my machines.
{ config, lib, pkgs, inputs, user, ... }:
{
  imports = [
    "./fonts.nix"
    "./locale.nix"
    "./networking.nix"
    "./packages.nix"
    "./programs.nix"
    "./services.nix"
    "./settings.nix"
    "./users.nix"
    "./xdg.nix"
    "./xorg.nix"
  ]
}