{
  networking = {
    useDHCP = false;
    firewall.enable = true;

    nameservers = [
      # Quad9 v4
      "9.9.9.9"
      "149.112.112.112"
      # Quad9 v6
      "2620:fe::fe"
      "2620:fe::9"
    ];

    networkmanager = {
      enable = true;
      dns = "none";
    };
  };

  services = {
    tailscale.enable = true;
  };
}
