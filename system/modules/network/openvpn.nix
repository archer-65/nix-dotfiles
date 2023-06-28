{
  config,
  pkgs,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.network.openvpn;
in {
  options.system.modules.network.openvpn = {
    work.enable = mkEnableOption "enable work vpn";
  };

  config = mkMerge [
    (mkIf cfg.work.enable {
      sops.secrets."work_client.ovpn" = {
        mode = "0400";
        sopsFile = ./work_client.ovpn;
        format = "binary";
      };

      services.openvpn.restartAfterSleep = false;
      services.openvpn.servers.work = let
        if-name = "work-vpn";
        set-dns = "${pkgs.systemd}/bin/resolvectl dns ${if-name}";
        set-domain = "${pkgs.systemd}/bin/resolvectl domain ${if-name}";
      in {
        # TOTP as password
        authUserPass.username = "mli";
        authUserPass.password = "";

        # Just disable this. Find a way to ask for TOTP with a dialog
        autoStart = false;

        config = ''
          dev ${if-name}
          dev-type tun
          config ${config.sops.secrets."work_client.ovpn".path}
        '';

        up = ''
          ${set-dns} 192.168.0.1
          ${set-domain} intranet.bit4id.com
        '';
        down = ''
          ${set-dns} ""
          ${set-domain} ""
        '';
      };
    })
  ];
}
