{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.credentials.mail-defaults;

  channelExtraConfig = {
    Create = "Near";
    SyncState = "*";
  };
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
      # lieer.enable = true;
      # notmuch.enable = true;
    };

    accounts.email.accounts = {
      # gmail = {
      #   primary = false;
      #   realName = "Mario Liguori";
      #   address = "mario.liguori.056@gmail.com";
      #   userName = "mario.liguori.056@gmail.com";
      #   flavor = "gmail.com";
      #   passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup gmail password";

      #   notmuch.enable = true;
      #   lieer = {
      #     enable = true;
      #     sync.enable = true;
      #     settings = {
      #       replace_slash_with_dot = true;
      #       ignore_tags = [ "new" ];
      #       ignore_remote_labels = [ ];
      #       drop_non_existing_label = true;
      #     };
      #   };
      # };

      gmail = rec {
        primary = true;
        flavor = "gmail.com";
        realName = "Mario Liguori";
        address = "mario.liguori.056@gmail.com";
        userName = address;
        passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup gmailPrimary password";

        mu.enable = true;

        msmtp.enable = true;

        mbsync = {
          enable = true;
          subFolders = "Verbatim";

          groups = {
            gmail = {
              channels = {
                inbox = {
                  farPattern = "INBOX";
                  nearPattern = "inbox";
                  extraConfig =
                    {
                      Sync = "All";
                      Expunge = "Both";
                    }
                    // channelExtraConfig;
                };

                sent = {
                  farPattern = "[Gmail]/Sent Mail";
                  nearPattern = "sent";
                  extraConfig =
                    {
                      Sync = "All";
                      Expunge = "Both";
                    }
                    // channelExtraConfig;
                };

                archive = {
                  farPattern = "[Gmail]/All Mail";
                  nearPattern = "archive";
                  extraConfig =
                    {
                      Sync = "All";
                      Expunge = "None";
                    }
                    // channelExtraConfig;
                };

                drafts = {
                  farPattern = "[Gmail]/Drafts";
                  nearPattern = "drafts";
                  extraConfig =
                    {
                      Sync = "Pull";
                      Expunge = "Both";
                    }
                    // channelExtraConfig;
                };

                trash = {
                  farPattern = "[Gmail]/Trash";
                  nearPattern = "trash";
                  extraConfig =
                    {
                      Sync = "All";
                      Expunge = "None";
                    }
                    // channelExtraConfig;
                };

                spam = {
                  farPattern = "[Gmail]/Spam";
                  nearPattern = "spam";
                  extraConfig =
                    {
                      Sync = "Pull";
                      Expunge = "Both";
                    }
                    // channelExtraConfig;
                };
              };
            };
          };
        };
      };

      unina = rec {
        primary = false;
        realName = "Mario Liguori";
        address = "mario.liguori6@studenti.unina.it";
        userName = address;

        passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup unina password";

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
