{
  config,
  lib,
  modulesPath,
  ...
}: {
  imports = [(modulesPath + "/profiles/qemu-guest.nix")];

  boot.initrd.availableKernelModules = ["ahci" "xhci_pci" "virtio_pci" "sr_mod" "virtio_blk"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/303afb46-1ca1-4de6-919c-7fd0544580b1";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/BA28-A14D";
    fsType = "vfat";
  };

  swapDevices = [{device = "/dev/disk/by-uuid/37a081d6-656e-4026-8f96-b09a1b61737a";}];

  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}
