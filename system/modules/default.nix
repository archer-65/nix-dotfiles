{utils}:
utils.lib.exportModules [
  ./core/boot.nix
  ./core/cachix.nix
  ./core/locale.nix
  ./core/network.nix
  ./core/packages.nix
  ./core/security.nix
  ./core/settings.nix
  ./core/user.nix

  ./desktop/base.nix
  ./desktop/portals.nix
  ./desktop/xorg.nix
  ./desktop/wayland.nix
  ./desktop/sddm.nix
  ./desktop/greetd.nix

  ./dev/adb.nix
  ./dev/virt-manager.nix
  ./dev/docker.nix

  ./hardware/audio.nix
  ./hardware/bluetooth.nix
  ./hardware/monitoring.nix
  ./hardware/razer.nix
  ./hardware/qmk.nix

  ./services/plex.nix
  ./services/jellyfin.nix
  ./services/printing.nix
  ./services/ssh.nix
  ./services/gpg.nix
  ./services/yubikey.nix

  ./shell/bash.nix
]
