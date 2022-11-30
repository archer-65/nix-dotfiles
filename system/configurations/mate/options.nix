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

    credentials = {
      ssh.enable = true;
      gpg.enable = true;
      yubikey.enable = true;
    };

    graphical.wayland.enable = true;
    graphical.greetd.enable = true;

    core = {
      boot.quietboot.enable = true;
      cachix.enable = true;
    };
  };

  services.flatpak.enable = true;
}
