# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/1bc3b052-bf39-4388-8313-926d6d83b0eb";
      fsType = "btrfs";
      options = [ "subvol=@" "autodefrag" "space_cache=v2" "noatime" "compress=zstd" "discard=async" ];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/1bc3b052-bf39-4388-8313-926d6d83b0eb";
      fsType = "btrfs";
      options = [ "subvol=@home" "autodefrag" "space_cache=v2" "noatime" "compress=zstd" "discard=async" ];
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-uuid/1bc3b052-bf39-4388-8313-926d6d83b0eb";
      fsType = "btrfs";
      options = [ "subvol=@nix" "autodefrag" "space_cache=v2" "relatime" "compress=zstd:2" "discard=async" ];
    };

  fileSystems."/var/log" =
    { device = "/dev/disk/by-uuid/1bc3b052-bf39-4388-8313-926d6d83b0eb";
      fsType = "btrfs";
      options = [ "subvol=@var_log" "autodefrag" "space_cache=v2" "noatime" "compress=zstd" "discard=async" ];
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/C273-0708";
      fsType = "vfat";
    };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp42s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp5s0.useDHCP = lib.mkDefault true;

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
