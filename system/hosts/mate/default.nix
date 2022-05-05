{ config, pkgs, ... }:

{
  imports = [ 
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot = {
   kernelPackages = pkgs.linuxPackages_latest;
   loader = {
     efi = {
       canTouchEfiVariables = true;
       efiSysMountPoint = "/boot";
     };
     systemd-boot.enable = true;
     timeout = 5;
   };
  };

  networking = {
    useDHCP = false;
    hostName = "mate";
    #wireless.enable = true;
    #wireless.userControlled.enable = true;
    interfaces = {
      wlp1s0.useDHCP = true;
    };
  };
}

