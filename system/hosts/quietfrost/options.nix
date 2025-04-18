# Mate, main laptop
{pkgs, ...}: {
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
      jellyfin.enable = true;
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
      nix-ld.enable = true;
    };

    shell = {
      bash.enable = true;
      zsh.enable = true;
    };
  };

  primaryUser.shell = pkgs.zsh;

  services.flatpak.enable = true;
  # TODO: Move this away
  hardware.graphics.extraPackages = with pkgs; [ rocmPackages.clr.icd ];
  systemd.tmpfiles.rules =
  let
    rocmEnv = pkgs.symlinkJoin {
      name = "rocm-combined";
      paths = with pkgs.rocmPackages; [
        rocblas
        hipblas
        clr
      ];
    };
  in [
    "L+    /opt/rocm   -    -    -     -    ${rocmEnv}"
  ];
}
