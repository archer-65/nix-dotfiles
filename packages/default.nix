{pkgs ? null}: {
  scripts = {
    volume = pkgs.callPackage ./scripts/volume.nix {};
    usedram = pkgs.callPackage ./scripts/usedram.nix {};
    usedcpu = pkgs.callPackage ./scripts/usedcpu.nix {};
    hwmon_devices = pkgs.callPackage ./scripts/hwmon_devices.nix {};
  };

  rofi-plugins = {
    emoji = pkgs.callPackage ./rofi/emoji.nix {};
    greenclip = pkgs.callPackage ./rofi/greenclip.nix {};
    launcher = pkgs.callPackage ./rofi/launcher.nix {};
    powermenu = pkgs.callPackage ./rofi/powermenu.nix {};
  };
}
