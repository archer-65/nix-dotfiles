{
  pkgs,
  lib,
  ...
}: {

  # Writable /etc/hosts
  environment.etc.hosts.mode = "0644";

  networking = {
    # global dhcp has been deprecated upstream
    # use the new networkd service instead of the legacy
    # "script-based" network setups. Host may contain individual
    # dhcp interfaces or systemd-networkd configurations in host
    # specific directories
    useDHCP = lib.mkForce false;
    useNetworkd = lib.mkForce true;

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
      dns = "systemd-resolved";
      wifi.powersave = true;
    };
  };

  services = {
    # DNS
    resolved = {
      enable = true;
      dnssec = "allow-downgrade";
      domains = [ "~." ];
      dnsovertls = "opportunistic";
    };

    # VPN
    tailscale.enable = true;
  };

  # According to 23.11 release notes, wait-online target has long been fixed.
  # Spoiler, it's not.
  systemd.network.wait-online.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;
}
