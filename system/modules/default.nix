inputs: {
  "core/boot" = import ./core/boot.nix inputs;
  "core/cachix" = import ./core/cachix.nix inputs;
  "core/locale" = import ./core/locale.nix inputs;
  "core/network" = import ./core/network.nix inputs;
  "core/packages" = import ./core/packages.nix inputs;
  "core/security" = import ./core/security.nix inputs;
  "core/settings" = import ./core/settings.nix inputs;
  "core/user" = import ./core/user.nix inputs;

  "desktop/base" = import ./desktop/base.nix inputs;
  "desktop/fonts" = import ./desktop/fonts.nix inputs;
  "desktop/portals" = import ./desktop/portals.nix inputs;
  "desktop/xorg" = import ./desktop/xorg.nix inputs;
  "desktop/wayland" = import ./desktop/wayland.nix inputs;

  "dev/adb" = import ./dev/adb.nix inputs;
  "dev/virt-manager" = import ./dev/virt-manager.nix inputs;

  "hardware/audio" = import ./hardware/audio.nix inputs;
  "hardware/bluetooth" = import ./hardware/bluetooth.nix inputs;
  "hardware/monitoring" = import ./hardware/monitoring.nix inputs;
  "hardware/razer" = import ./hardware/razer.nix inputs;

  "services/flatpak" = import ./services/flatpak.nix inputs;
  "services/plex" = import ./services/plex.nix inputs;
  "services/printing" = import ./services/printing.nix inputs;
  "services/ssh" = import ./services/ssh.nix inputs;
  "services/gpg" = import ./services/gpg.nix inputs;

  "shell/bash" = import ./shell/bash.nix inputs;
}
