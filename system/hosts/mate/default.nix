{
  pkgs,
  lib,
  ...
}: {
  imports = [./hardware-configuration.nix ./options.nix];

  # Kernel related

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.kernelModules = ["amdgpu"];
  boot.kernelParams = ["idle=nomwait" "iommu=pt"];

  # General EFI settings
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot/efi";
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
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [amdvlk vaapiVdpau libvdpau-va-gl];
  };

  services.xserver = {
    enable = true;
    # Disable `lightdm` because it is enabled by default sometimes (e.g. greetd with also `xserver` option enabled).
    displayManager.lightdm.enable = lib.mkForce false;
    videoDrivers = ["amdgpu"];
  };

  services.auto-cpufreq = {enable = true;};
}
