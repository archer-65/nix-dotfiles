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

    # Needed for yubikey to work
    environment.shellInit = ''
      export GPG_TTY="$(tty)"
      gpg-connect-agent /bye
      export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
    '';

    # Setup yubikey
    services.udev.packages = [pkgs.yubikey-personalization];
    services.pcscd.enable = true;
  };
}
