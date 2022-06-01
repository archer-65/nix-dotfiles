_:
{ config, lib, pkgs, ... }:

{
  # This global flag is deprecated
  networking.useDHCP = false;
  networking.networkmanager = {
    enable = true;
    #packages = [];
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  networking.firewall.enable = true;
}
