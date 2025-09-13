{
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ./options.nix
    inputs.disko.nixosModules.disko
  ];

  inherit (import ./disko.nix { disks = [ "/dev/sda" ]; }) disko;

  # https://github.com/nix-community/nixos-anywhere/issues/178
  security.sudo.wheelNeedsPassword = lib.mkForce false;
}
