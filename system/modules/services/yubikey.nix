_:
{ pkgs, config, options, lib, ... }:

with lib;
let
  cfg = config.modules.services.yubikey;
in {
  options.modules.services.yubikey = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    modules.services.gpg.enable = true;

    # Needed for yubikey to work
    environment.shellInit = ''
      export GPG_TTY="$(tty)"
      gpg-connect-agent /bye
      export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
    '';

    # Setup yubikey
    services.udev.packages = [ pkgs.yubikey-personalization ];
    services.pcscd.enable = true;
    environment.systemPackages = [ pkgs.yubioath-desktop ];
  };
}
