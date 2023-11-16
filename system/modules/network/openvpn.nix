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
      in {
        # TOTP as password
        authUserPass.username = "mli";
        authUserPass.password = "";

        # Just disable this. Find a way to ask for TOTP with a dialog
        autoStart = false;

        config = ''
          script-security 2
          up ${pkgs.update-systemd-resolved}/libexec/openvpn/update-systemd-resolved
          up-restart
          down ${pkgs.update-systemd-resolved}/libexec/openvpn/update-systemd-resolved
          down-pre
          config ${config.sops.secrets."work_client.ovpn".path}
          dev ${if-name}
          dev-type tun
        '';
      };
    })
  ];
}
