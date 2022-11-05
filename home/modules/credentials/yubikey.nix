{
  pkgs,
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.home.modules.credentials.yubikey;
in {
  options.home.modules.credentials.yubikey = {
    enable = mkEnableOption "yubikey user additions";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [yubioath-desktop yubikey-manager yubikey-manager-qt yubikey-touch-detector yubikey-personalization-gui];

    # https://github.com/fishhead108/sif/blob/4b0a282ed03892b8537071ad31d14483a4987d62/home/users/fishhead/configs/keyring.nix
    systemd.user.sockets.yubikey-touch-detector = {
      Unit.Description = "Unix socket activation for YubiKey touch detector service";
      Socket = {
        ListenStream = "%t/yubikey-touch-detector.socket";
        RemoveOnStop = true;
      };
      Install.WantedBy = ["sockets.target"];
    };

    systemd.user.services.yubikey-touch-detector = {
      Unit = {
        Description = "Detects when your YubiKey is waiting for a touch";
        Requires = "yubikey-touch-detector.socket";
      };
      Service = {
        ExecStart = "${pkgs.yubikey-touch-detector}/bin/yubikey-touch-detector --libnotify";
        EnvironmentFile = "-%E/yubikey-touch-detector/service.conf";
      };
      Install = {
        Also = "yubikey-touch-detector.socket";
        WantedBy = ["default.target"];
      };
    };
  };
}
