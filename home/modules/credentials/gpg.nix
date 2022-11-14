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
            url = "https://keybase.io/archer65/pgp_keys.asc";
            sha256 = "sha256:1ssx47r0rwp06g3saf3miff31q9qxsygz9j891lm43axd5m2an5v";
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
