{
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.services.ssh;
in {
  options.system.modules.services.ssh = {
    enable = mkEnableOption "ssh and a secure configuration";
  };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      startWhenNeeded = true;
      openFirewall = true;

      passwordAuthentication = false;
      allowSFTP = false;
      kbdInteractiveAuthentication = false;

      forwardX11 = false;
      extraConfig = ''
        AllowTcpForwarding yes
        AllowAgentForwarding no
        AllowStreamLocalForwarding no
        AuthenticationMethods publickey
      '';
    };

    services.sshd.enable = true;
  };
}
