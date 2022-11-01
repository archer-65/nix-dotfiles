{
  modules = {
    hardware = { audio.enable = true; };

    services = { ssh.enable = true; };

    desktop.xorg.enable = true;

    core.cachix.enable = true;
  };
}
