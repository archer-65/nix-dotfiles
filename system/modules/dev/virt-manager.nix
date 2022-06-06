_:
{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.dev.virt-manager;
in {
  options.modules.dev.virt-manager = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    virtualisation.libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        ovmf = {
          enable = true;
          packages = [ pkgs.OVMFFull ];
        };
        swtpm.enable = true;
      };
    };

    environment.systemPackages = with pkgs; [ virt-manager swtpm ];

    user.extraGroups = [ "libvirtd" ];
  };
}
