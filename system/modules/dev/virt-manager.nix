{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.virt-manager;
in {
  options.modules.dev.virt-manager = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    virtualisation.libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        ovmf = {
          enable = true;
          package = pkgs.OVMFFull;
        };
        swtpm.enable = true;
      };
    };

    environment.systemPackages = with pkgs; [ virt-manager swtpm ];
    
    user.extraGroups = [ "libvirtd" ];
  };
}
