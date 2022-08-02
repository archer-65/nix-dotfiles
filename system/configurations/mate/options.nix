# Mate, main laptop
{ ... }: {
  modules = {
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      monitoring.enable = true;
    };

    services = {
      ssh.enable = true;
      gpg.enable = true;
    };

    desktop.xorg.enable = true;
    desktop.sddm.enable = true;

    core = {
      boot.splashBoot.enable = true;
      cachix.enable = true;
    };
  };

  services.flatpak.enable = true;
}
