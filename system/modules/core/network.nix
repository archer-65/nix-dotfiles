_:
{ ... }:

{
  # This global flag is deprecated
  networking.useDHCP = false;
  networking.networkmanager = {
    enable = true;
    #packages = [];

    insertNameservers = [ "208.67.222.222" "208.67.220.220" ];
  };
  networking.nameservers = [ "208.67.222.222" "208.67.220.220" ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  networking.firewall.enable = true;
}
