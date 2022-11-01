{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.credentials.mail-defaults;
in {
  options.home.modules.credentials.mail-defaults = {
    enable = mkEnableOption "mail support";
  };

  config = mkIf cfg.enable {
    accounts.email.maildirBasePath = "${config.home.homeDirectory}/mails";

    programs = {
      mbsync.enable = true;
      mu.enable = true;
      msmtp.enable = true;
    };

    accounts.email.accounts = {
      gmailPrimary = {
        primary = true;
        realName = "Mario Liguori";
        address = "mario.liguori.056@gmail.com";
        userName = "mario.liguori.056@gmail.com";

        flavor = "gmail.com";
        passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup gmailPrimary password";

        maildir.path = "GmailPrimary";

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

        passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup unina password";

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
          patterns = ["*"];
          create = "both";
          expunge = "both";
          extraConfig = {channel = {Sync = "All";};};
        };
      };
    };
  };
}
