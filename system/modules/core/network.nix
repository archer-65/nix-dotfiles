{
  pkgs,
  lib,
  ...
}: {

  # Writable /etc/hosts
  environment.etc.hosts.mode = "0644";

  networking = {
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
    resolved.enable = true;

    # VPN
    tailscale.enable = true;
  };

  # According to 23.11 release notes, wait-online target has long been fixed.
  # Spoiler, it's not.
  systemd.network.wait-online.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;
}
