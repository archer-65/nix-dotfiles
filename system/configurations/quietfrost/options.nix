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
    };

    dev = {
      adb.enable = true;
    };

    media = {
      plex.enable = true;
    };

    services = {
      ssh.enable = true;
      gpg.enable = true;
      flatpak.enable = true;
      printing.enable = true;
    };

    desktop.xorg.enable = true;

    core = {
      boot.splashBoot.enable = true;
      cachix.enable = true;
    };
  };
}
