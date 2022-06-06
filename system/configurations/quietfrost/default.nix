{ pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ./options.nix ];

  # Kernel related
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.kernelModules = [ "amdgpu" ];
  boot.supportedFilesystems = [ "ntfs" ];

  # General EFI settings
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot/efi";
  };
  boot.loader.timeout = 5;

  # Use the systemd-boot EFI boot loader.
  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    device = "nodev";
    gfxmodeEfi = "3440x1440";
    useOSProber = true;
  };

  networking.hostName = "quietfrost";
  networking.interfaces.enp42s0.useDHCP = true;

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

  boot.kernelParams = [ "zswap.enabled=0" ];
  zramSwap = {
    enable = true;
    memoryPercent = 40;
    priority = 10;
  };
}
