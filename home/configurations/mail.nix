{ config, pkgs, ... }:
{
  accounts.email.accounts.live = {
    primary = true;
    realName = "Mario Liguori";
    address = "mariogt2009@live.it";
    userName = "mariogt2009@live.it";
    passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup live password";

    imap = {
      host = "outlook.office365.com";
      port = 993;
      tls = {
        enable = true;
      };
    };

    smtp = {
      host = "smtp.fastmail.com";
      port = 465;
      tls = {
        enable = true;
      };
    };

    mu.enable = true;
    msmtp.enable = true;
    mbsync = {
      enable = true;
      create = "both";
      expunge = "both";
      extraConfig = {
        channel = {
          Sync = "All";
        };
        account = {
          Timeout = 120;
          PipelineDepth = 50;
        };
      };
    };
  };

  accounts.email.accounts.gmail = {
    primary = false;
    realName = "Mario Liguori";
    address = "mariogt2009@gmail.com";
    userName = "mariogt2009@gmail.com";
    flavor = "gmail.com";
    passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup gmail password";

    mu.enable = true;
    msmtp.enable = true;
    mbsync = {
      enable = true;
      patterns = [ "*" "![Gmail]*" "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/All Mail" ];
      create = "both";
    };
  };

  accounts.email.accounts.unina = {
    primary = false;
    realName = "Mario Liguori";
    address = "mario.liguori6@studenti.unina.it";
    userName = "mario.liguori6@studenti.unina.it";
    passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup unina password";

    imap = {
      host = "outlook.office365.com";
      port = 993;
      tls = {
        enable = true;
      };
    };

    smtp = {
      host = "smtp.fastmail.com";
      port = 465;
      tls = {
        enable = true;
      };
    };

    mu.enable = true;
    msmtp.enable = true;
    mbsync = {
      enable = true;
      patterns = [ "*" ];
      create = "both";
      expunge = "both";
      extraConfig = {
        channel = {
          Sync = "All";
        };
      };
    };
  };

  programs = {
    mbsync.enable = true;
    mu.enable = true;
    msmtp.enable = true;
  };
}