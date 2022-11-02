{pkgs ? null}: {
  script-volume = pkgs.callPackage ./scripts/volume.nix {};
  script-usedram = pkgs.callPackage ./scripts/usedram.nix {};
  script-usedcpu = pkgs.callPackage ./scripts/usedcpu.nix {};
  script-hwmon_devices = pkgs.callPackage ./scripts/hwmon_devices.nix {};

  script-emoji = pkgs.callPackage ./rofi/emoji.nix {};
  script-greenclip = pkgs.callPackage ./rofi/greenclip.nix {};
  script-launcher = pkgs.callPackage ./rofi/launcher.nix {};
  script-powermenu = pkgs.callPackage ./rofi/powermenu.nix {};
}
