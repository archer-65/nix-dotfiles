{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.dev.virt-manager;
in {
  options.system.modules.dev.virt-manager = {
    enable = mkEnableOption "virt-manager qemu-kvm";
  };

  config = mkIf cfg.enable {
    virtualisation.libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        ovmf = {
          enable = true;
          packages = [pkgs.OVMFFull];
        };
        swtpm.enable = true;
      };
    };

    environment.systemPackages = with pkgs; [virt-manager swtpm];

    primaryUser.extraGroups = ["libvirtd"];
  };
}
