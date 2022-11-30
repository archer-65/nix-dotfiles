{pkgs ? null}: {
  script-emoji = pkgs.callPackage ./rofi/emoji.nix {};
  script-launcher = pkgs.callPackage ./rofi/launcher.nix {};
  script-greenclip = pkgs.callPackage ./rofi/greenclip {};
  script-powermenu = pkgs.callPackage ./rofi/powermenu/powermenu.nix {};
  script-usedram = pkgs.callPackage ./rofi/powermenu/usedram.nix {};
  script-usedcpu = pkgs.callPackage ./rofi/powermenu/usedcpu.nix {};

  script-volume = pkgs.callPackage ./scripts/volume {};
  script-theme-toggle = pkgs.callPackage ./scripts/theme-toggle {};
}
