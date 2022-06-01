# This configuration files contains common settings (of the system)
# between all my machines.
{ config, lib, pkgs, inputs, ... }: {
  imports = [
    ./boot.nix
    ./cachix.nix
    ./fonts
    ./locale.nix
    ./networking.nix
    ./packages.nix
    ./programs.nix
    ./security.nix
    ./services.nix
    ./settings.nix
    ./users.nix
    ./xdg.nix
    ./xorg.nix
  ];
}
