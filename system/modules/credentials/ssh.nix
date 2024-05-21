{
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.credentials.ssh;
in {
  options.system.modules.credentials.ssh = {
    enable = mkEnableOption "ssh and a secure configuration";
  };

  config = mkIf cfg.enable {
    # x11-askpass is a bad default.
    programs.ssh.askPassword = "";

    services.openssh = {
      enable = true;
      # startWhenNeeded = true;
      openFirewall = true;

      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
        KbdInteractiveAuthentication = false;
        X11Forwarding = false;
      };

      allowSFTP = false;

      extraConfig = ''
        AllowTcpForwarding no
        AllowAgentForwarding no
        AllowStreamLocalForwarding no
        AuthenticationMethods publickey
      '';

      hostKeys = [
        {
          path = "/etc/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
    };
  };
}
