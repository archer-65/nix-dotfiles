{
  "core/quietboot" = import ./core/quietboot.nix;
  "core/cachix" = import ./core/cachix.nix;
  "core/locale" = import ./core/locale.nix;
  "core/network" = import ./core/network.nix;
  "core/packages" = import ./core/packages.nix;
  "core/security" = import ./core/security.nix;
  "core/settings" = import ./core/settings.nix;
  "core/user" = import ./core/user.nix;

  "graphical/common" = import ./graphical/common.nix;
  "graphical/portals" = import ./graphical/portals.nix;
  "graphical/xorg" = import ./graphical/xorg.nix;
  "graphical/wayland" = import ./graphical/wayland.nix;
  "graphical/sddm" = import ./graphical/sddm.nix;
  "graphical/greetd" = import ./graphical/greetd.nix;
  "graphical/gnome" = import ./graphical/gnome.nix;
  "graphical/displaylink" = import ./graphical/displaylink.nix;

  "dev/adb" = import ./dev/adb.nix;
  "dev/virt-manager" = import ./dev/virt-manager.nix;
  "dev/docker" = import ./dev/docker.nix;
  "dev/manpages" = import ./dev/manpages.nix;

  "hardware/audio" = import ./hardware/audio.nix;
  "hardware/bluetooth" = import ./hardware/bluetooth.nix;
  "hardware/monitoring" = import ./hardware/monitoring.nix;
  "hardware/razer" = import ./hardware/razer.nix;
  "hardware/qmk" = import ./hardware/qmk.nix;

  "services/plex" = import ./services/plex.nix;
  "services/jellyfin" = import ./services/jellyfin.nix;
  "services/printing" = import ./services/printing.nix;

  "credentials/ssh" = import ./credentials/ssh.nix;
  "credentials/gpg" = import ./credentials/gpg.nix;
  "credentials/yubikey" = import ./credentials/yubikey.nix;

  "shell/bash" = import ./shell/bash.nix;
  "shell/zsh" = import ./shell/zsh.nix;

  "network/openvpn" = import ./network/openvpn.nix;
}
