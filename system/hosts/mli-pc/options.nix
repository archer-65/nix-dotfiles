# Work laptop laptop
{pkgs, ...}: {
  system.modules = {
    hardware = {
      audio.enable = true;
      monitoring = {
        enable = true;
      };
      qmk.enable = true;
    };

    dev = {
      adb.enable = true;
      docker.enable = true;
      manpages.enable = true;
    };

    credentials = {
      ssh.enable = true;
      gpg.enable = true;
      yubikey.enable = true;
    };

    # graphical.xorg.enable = true;
    # graphical.wayland.enable = true;
    # graphical.greetd.enable = true;
    # graphical.sddm.enable = true;

    core = {
      boot.quietboot.enable = true;
      cachix.enable = true;
    };

    shell = {
      bash.enable = true;
      zsh.enable = true;
    };
  };

  primaryUser.shell = pkgs.zsh;

  services.flatpak.enable = true;
}
