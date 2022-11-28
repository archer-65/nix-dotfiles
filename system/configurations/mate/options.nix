{
  system.modules = {
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      monitoring.enable = true;
    };

    dev = {
      docker.enable = true;
      manpages.enable = true;
    };

    services = {
      ssh.enable = true;
      gpg.enable = true;
      yubikey.enable = true;
    };

    desktop.xorg.enable = true;
    desktop.wayland.enable = true;
    # desktop.sddm.enable = true;
    desktop.greetd.enable = true;

    core = {
      boot.quietboot.enable = true;
      cachix.enable = true;
    };
  };

  services.flatpak.enable = true;
}
