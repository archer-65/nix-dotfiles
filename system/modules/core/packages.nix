_:
{ pkgs, lib, ... }:

{
  # System-wide packages
  environment = {
    defaultPackages = lib.mkForce [];
    systemPackages = with pkgs; [ killall procps wget git vim ];
  };
}
