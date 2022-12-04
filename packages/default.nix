{pkgs ? null}: {
  rofi-powermenu = pkgs.callPackage ./rofi-powermenu {};
  theme-toggle = pkgs.callPackage ./theme-toggle {};
}
