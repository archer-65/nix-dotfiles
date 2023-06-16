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

      sops = {
        secrets."work_client.ovpn" = {
        mode = "0400";
        # owner = config.users.users.root.name;
        # group = config.users.users.nobody.group;
        sopsFile = ./work_client.ovpn;
        format = "binary";
        };
      };

      services.openvpn.servers.work = let
        if-name = "work-vpn";
        set-dns = "${pkgs.systemd}/bin/resolvectl dns ${if-name}";
        set-domain = "${pkgs.systemd}/bin/resolvectl domain ${if-name}";
      in {
        authUserPass.username = "mli";
        authUserPass.password = "";
        updateResolvConf = true;
        up = ''
          ${set-dns} 192.168.0.1
          ${set-domain} intranet.bit4id.com
        '';
        down = ''
          ${set-dns} ""
          ${set-domain} ""
        '';
        config = ''
          dev ${if-name}
          dev-type tun
          config ${config.sops.secrets."work_client.ovpn".path}
        '';
      };
    })
  ];
}
