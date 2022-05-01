{ config, pkgs, ... }: {

  imports  = [
    ./hardware-configuration.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
      grub = {
        enable = true;
        version = 2;
        efiSupport = true;
        device = "nodev";
      };
      timeout = 5;
    };
  };

  networking = {
    useDHCP = false;
    hostName = "nixos";
    interfaces = {
      enp1s0.useDHCP = true;
    };
  };
}
