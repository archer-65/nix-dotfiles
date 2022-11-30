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
    services.openssh = {
      enable = true;
      startWhenNeeded = true;
      openFirewall = true;

      passwordAuthentication = false;
      permitRootLogin = "no";

      kbdInteractiveAuthentication = false;

      allowSFTP = false;

      forwardX11 = false;

      extraConfig = ''
        AllowTcpForwarding no
        AllowAgentForwarding no
        AllowStreamLocalForwarding no
        AuthenticationMethods publickey
      '';
    };
  };
}
