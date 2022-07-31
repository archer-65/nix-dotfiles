# Mate, main laptop
{ ... }: {
  modules = {
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
      virt-manager.enable = true;
    };

    media = { plex.enable = true; };

    services = {
      ssh.enable = true;
      gpg.enable = true;
      printing.enable = true;
    };

    desktop.xorg.enable = true;
    desktop.wayland.enable = true;

    core = {
      # boot.splashBoot.enable = true;
      cachix.enable = true;
    };
  };

  services.flatpak.enable = true;
}
