{pkgs ? null}: {
  rofi-powermenu = pkgs.callPackage ./rofi-powermenu {};
  aws-mfa = pkgs.callPackage ./aws-mfa {};
}
