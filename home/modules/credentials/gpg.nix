{
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
    signing = {
      enable = mkEnableOption "gpg signing";
      key = mkOption {
        description = "gpg signing key";
        type = types.str;
      };
    };
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
      pinentryFlavor = "gnome3";
      sshKeys = ["19953CB0EC3A2941EF36DA2D7BDA72F1E2404770"];
    };
  };
}
