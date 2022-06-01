{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ./options.nix ];

  # Kernel related
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.kernelModules = [ "amdgpu" ];

  # General EFI settings
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot";
  };
  boot.loader.timeout = 5;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot = {
    enable = true;
    editor = false;
  };

  # WiFi
  networking.interfaces.wlp1s0.useDHCP = true;

  # Graphics
  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [ amdvlk ];
  };

  services.xserver = {
    enable = true;
    videoDrivers = [ "amdgpu" ];
  };

  services.auto-cpufreq = { enable = true; };
}

