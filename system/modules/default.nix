{
  "core/boot" = import ./core/boot.nix;
  "core/cachix" = import ./core/cachix.nix;
  "core/locale" = import ./core/locale.nix;
  "core/network" = import ./core/network.nix;
  "core/packages" = import ./core/packages.nix;
  "core/security" = import ./core/security.nix;
  "core/settings" = import ./core/settings.nix;
  "core/user" = import ./core/user.nix;

  "desktop/base" = import ./desktop/base.nix;
  "desktop/portals" = import ./desktop/portals.nix;
  "desktop/xorg" = import ./desktop/xorg.nix;
  "desktop/wayland" = import ./desktop/wayland.nix;
  "desktop/sddm" = import ./desktop/sddm.nix;
  "desktop/greetd" = import ./desktop/greetd.nix;

  "dev/adb" = import ./dev/adb.nix;
  "dev/virt-manager" = import ./dev/virt-manager.nix;
  "dev/docker" = import ./dev/docker.nix;

  "hardware/audio" = import ./hardware/audio.nix;
  "hardware/bluetooth" = import ./hardware/bluetooth.nix;
  "hardware/monitoring" = import ./hardware/monitoring.nix;
  "hardware/razer" = import ./hardware/razer.nix;
  "hardware/qmk" = import ./hardware/qmk.nix;

  "services/plex" = import ./services/plex.nix;
  "services/jellyfin" = import ./services/jellyfin.nix;
  "services/printing" = import ./services/printing.nix;
  "services/ssh" = import ./services/ssh.nix;
  "services/gpg" = import ./services/gpg.nix;
  "services/yubikey" = import ./services/yubikey.nix;

  "shell/bash" = import ./shell/bash.nix;
}
