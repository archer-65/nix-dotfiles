# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{
  config,
  lib,
  modulesPath,
  ...
}: {
  imports = [(modulesPath + "/installer/scan/not-detected.nix")];

  boot.initrd.availableKernelModules = ["xxhash" "vmd" "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  boot.initrd = {
    luks.devices."cryptroot" = {
      device = "/dev/disk/by-uuid/8554e9e1-76a7-4b50-95e7-aced21abe271";
      bypassWorkqueues = true;
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/75aaa4a8-cae5-4d53-893b-100c2807af7a";
    fsType = "btrfs";
    options = [
      "subvol=@"
      "autodefrag"
      "space_cache=v2"
      "noatime"
      "compress=zstd:2"
      "discard=async"
    ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/75aaa4a8-cae5-4d53-893b-100c2807af7a";
    fsType = "btrfs";
    options = [
      "subvol=@home"
      "autodefrag"
      "space_cache=v2"
      "noatime"
      "compress=zstd:2"
      "discard=async"
    ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/75aaa4a8-cae5-4d53-893b-100c2807af7a";
    fsType = "btrfs";
    options = [
      "subvol=@nix"
      "autodefrag"
      "space_cache=v2"
      "noatime"
      "compress=zstd:2"
      "discard=async"
    ];
  };

  fileSystems."/var/log" = {
    device = "/dev/disk/by-uuid/75aaa4a8-cae5-4d53-893b-100c2807af7a";
    fsType = "btrfs";
    options = [
      "subvol=@var_log"
      "autodefrag"
      "space_cache=v2"
      "noatime"
      "compress=zstd:2"
      "discard=async"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/38D9-1033";
    fsType = "vfat";
  };

  services.btrfs.autoScrub = {
    enable = true;
    interval = "monthly";
  };

  swapDevices = [];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}
