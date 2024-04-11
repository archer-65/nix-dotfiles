{pkgs ? null}: {
  rofi-powermenu = pkgs.callPackage ./rofi-powermenu {};
  rofi-powermenu-wayland = pkgs.callPackage ./rofi-powermenu {backend = "wayland";};
  aws-mfa = pkgs.callPackage ./aws-mfa {};
}
