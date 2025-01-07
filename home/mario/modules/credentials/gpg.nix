{
  pkgs,
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.credentials.gpg;
in {
  options.mario.modules.credentials.gpg = {
    enable = mkEnableOption "gpg-agent user configuration";
  };

  config = mkIf cfg.enable {
    programs.gpg = {
      enable = true;
      publicKeys = [
        {
          source = builtins.fetchurl {
            # Without fingerprint the hash changes
            url = "https://keybase.io/archer65/pgp_keys.asc?fingerprint=2080983ce6822c29c09f8d5ebac570b2172822a3";
            sha256 = "sha256:0yrsry26mfzrdymv0i7f8jhkkr3j089619lr6bmz0l6n4siixxk3";
          };
          trust = 5;
        }
      ];
    };

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryPackage =
        if pkgs.stdenv.isDarwin
        then pkgs.pinentry_mac
        else pkgs.pinentry-gnome3;

      # NOTE: This should not be needed anymore, it is commented out until (and if) I find issues.
      #       - GPG on smartcards is automatically exposed as SSH key
      #	- Deprecated for GPG keys as the attribute 'use-for-ssh' exists
      #       - Usable by `ssh-add` for SSH keys
      #
      # https://www.gnupg.org/documentation/manuals/gnupg/Agent-Configuration.html#index-sshcontrol
      #
      # sshKeys = ["19953CB0EC3A2941EF36DA2D7BDA72F1E2404770"];
    };
  };
}
