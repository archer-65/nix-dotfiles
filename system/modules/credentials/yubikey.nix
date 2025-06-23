{
  pkgs,
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.credentials.yubikey;
in {
  options.system.modules.credentials.yubikey = {
    enable = mkEnableOption "yubikey";
  };

  config = mkIf cfg.enable {
    # Needed
    system.modules.credentials.gpg.enable = true;

    services.udev.packages = [pkgs.yubikey-personalization];
    services.pcscd.enable = true;
    environment.systemPackages = with pkgs; [yubikey-manager yubioath-flutter yubikey-touch-detector];

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
