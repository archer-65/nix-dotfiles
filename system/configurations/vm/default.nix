{ config, pkgs, ... }: {

  imports  = [
    ./hardware-configuration.nix
    ./options.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot";
  };
  
  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    device = "nodev";
  };

  boot.loader.timeout = 5;

  networking.interfaces.enp1s0.useDHCP = true;

  services.xserver.resolutions = [
    { x = 1920; y = 1080; }
  ];
}
