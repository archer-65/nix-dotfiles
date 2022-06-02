_:
{ pkgs, ... }:

{
  # System-wide packages
  environment = {
    systemPackages = with pkgs; [ killall procps wget git vim ];
  };
}
