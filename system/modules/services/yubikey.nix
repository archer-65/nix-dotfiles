{
  pkgs,
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.services.yubikey;
in {
  options.system.modules.services.yubikey = {
    enable = mkEnableOption "yubikey";
  };

  config = mkIf cfg.enable {
    system.modules.services.gpg.enable = true;

    # Needed for yubikey to work (?)
    # environment.shellInit = ''
    #  export GPG_TTY="$(tty)"
    #  gpg-connect-agent /bye
    #  export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
    # '';

    # Setup yubikey
    services.udev.packages = [pkgs.yubikey-personalization];
    services.pcscd.enable = true;
    environment.systemPackages = with pkgs; [yubikey-manager yubikey-manager-qt yubikey-touch-detector yubikey-personalization-gui];

    # https://github.com/mstrangfeld/nixos-configuration/blob/15f8d6bec7ce27d0cf3fefec4be96ad6bee9522f/modules/desktop/yubikey/default.nix
    systemd.user.services.yubikey-touch-detector = {
      description = "Detects when your YubiKey is waiting for a touch";
      requires = ["yubikey-touch-detector.socket"];
      path = with pkgs; [gnupg];
      serviceConfig = {
        ExecStart = "${pkgs.yubikey-touch-detector}/bin/yubikey-touch-detector -v --libnotify";
      };
      wantedBy = ["default.target"];
    };

    systemd.user.sockets.yubikey-touch-detector = {
      description = "Unix socket activation for YubiKey touch detector service";
      socketConfig = {
        ListenStream = "%t/yubikey-touch-detector.socket";
        RemoveOnStop = "yes";
      };
      wantedBy = ["default.target"];
    };
  };
}
