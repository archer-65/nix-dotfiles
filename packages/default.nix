{pkgs ? null}: {
  rofi-powermenu = pkgs.callPackage ./rofi-powermenu {};
  rofi-powermenu-wayland = pkgs.callPackage ./rofi-powermenu {backend = "wayland";};

  meridian-plugin-opencode-scrub = pkgs.callPackage ./meridian-plugin-opencode-scrub {};
}
