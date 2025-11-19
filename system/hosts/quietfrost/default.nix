{
  pkgs,
  lib,
  ...
}: {
  imports = [./hardware-configuration.nix ./options.nix];

  # Kernel related
  hardware.amdgpu.initrd.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    "initcall_blacklist=acpi_cpufreq_init"
    "amd_pstate=active"
    "amdgpu.dcfeaturemask=0x8"
    "zswap.enabled=0"
  ];

  boot.initrd.kernelModules = [];
  boot.initrd.availableKernelModules = [];
  boot.supportedFilesystems = ["ntfs"];

  # General EFI settings
  boot.loader.efi = {
    efiSysMountPoint = "/boot/efi";
  };
  boot.loader.timeout = 5;

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
      device = "nodev";
      gfxmodeEfi = "3440x1440";
      useOSProber = true;
    };
  };

  networking.interfaces.enp42s0.useDHCP = true;

  # Graphics
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [libva-vdpau-driver libvdpau-va-gl];
  };

  services.xserver = {
    enable = true;
    # Disable `lightdm` because it is enabled by default sometimes (e.g. greetd with also `xserver` option enabled).
    displayManager.lightdm.enable = lib.mkForce false;
    videoDrivers = ["amdgpu"];
  };

  # Firmware updater
  services.fwupd.enable = true;

  zramSwap = {
    enable = true;
    memoryPercent = 40;
    priority = 10;
  };

  services.libinput = {
    enable = true;

    mouse = {
      buttonMapping = "1 2 3 4 5 6 7 8 9 10 11 12";
      scrollMethod = "button";
      scrollButton = 12;
      additionalOptions = ''
        MatchProduct "ELECOM TrackBall Mouse HUGE TrackBall"
      '';
    };
  };

  environment.systemPackages = [pkgs.compsize];
}
