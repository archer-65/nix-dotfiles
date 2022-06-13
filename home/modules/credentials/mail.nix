_:
{ config, options, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.credentials.mail-defaults;
in {
  options.user-modules.credentials.mail-defaults = with types; {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    accounts.email.maildirBasePath = "${config.home.homeDirectory}/mails";

    programs = {
      mbsync.enable = true;
      mu.enable = true;
      msmtp.enable = true;
    };

    accounts.email.accounts = {
      outlook = {
        primary = true;
        realName = "Mario Liguori";
        address = "mariogt2009@live.it";
        userName = "mariogt2009@live.it";

        passwordCommand =
          "${pkgs.libsecret}/bin/secret-tool lookup live password";

        maildir.path = "Outlook";

        imap = {
          host = "outlook.office365.com";
          port = 993;
          tls.enable = true;
        };

        smtp = {
          host = "smtp-mail.outlook.com";
          port = 587;
          tls.enable = true;
        };

        mu.enable = true;

        msmtp.enable = true;

        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          extraConfig = {
            channel = { Sync = "All"; };
            account = {
              Timeout = 120;
              PipelineDepth = 50;
            };
          };
        };
      };

      gmail = {
        primary = false;
        realName = "Mario Liguori";
        address = "mariogt2009@gmail.com";
        userName = "mariogt2009@gmail.com";

        flavor = "gmail.com";
        passwordCommand =
          "${pkgs.libsecret}/bin/secret-tool lookup gmail password";

        maildir.path = "Gmail";

        mu.enable = true;

        msmtp.enable = true;

        mbsync = {
          enable = true;
          patterns = [
            "*"
            "![Gmail]*"
            "[Gmail]/Trash"
            "[Gmail]/Drafts"
            "[Gmail]/Sent Mail"
            "[Gmail]/Starred"
            "[Gmail]/All Mail"
          ];
          create = "both";
        };
      };

      unina = {
        primary = false;
        realName = "Mario Liguori";
        address = "mario.liguori6@studenti.unina.it";
        userName = "mario.liguori6@studenti.unina.it";

        passwordCommand =
          "${pkgs.libsecret}/bin/secret-tool lookup unina password";

        maildir.path = "Unina";

        imap = {
          host = "studenti.unina.it";
          port = 993;
          tls.enable = true;
        };

        smtp = {
          host = "studenti.unina.it";
          port = 465;
          tls.enable = true;
        };

        mu.enable = true;

        msmtp.enable = true;

        mbsync = {
          enable = true;
          patterns = [ "*" ];
          create = "both";
          expunge = "both";
          extraConfig = { channel = { Sync = "All"; }; };
        };
      };
    };
  };
}
