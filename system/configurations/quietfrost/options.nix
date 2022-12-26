# Mate, main laptop
{
  system.modules = {
    hardware = {
      audio.enable = true;
      monitoring = {
        enable = true;
        corectrl.enable = true;
      };
      razer.enable = true;
      qmk.enable = true;
    };

    dev = {
      adb.enable = true;
      docker.enable = true;
      virt-manager.enable = true;
      manpages.enable = true;
    };

    media = {
      plex.enable = true;
    };

    credentials = {
      ssh.enable = true;
      gpg.enable = true;
      yubikey.enable = true;
    };

    services = {
      printing.enable = true;
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
