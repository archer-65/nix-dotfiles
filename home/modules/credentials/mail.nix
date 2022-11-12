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

    # Read https://vxlabs.com/2021/03/21/mbsync-copyarrivaldate-yes/
    CopyArrivalDate = "yes";
  };
in {
  options.home.modules.credentials.mail-defaults = {
    enable = mkEnableOption "mail support";
  };

  config = mkIf cfg.enable {
    accounts.email.maildirBasePath = "${config.home.homeDirectory}/mails";

    programs = {
      mbsync.enable = true;
      msmtp.enable = true;
      # lieer.enable = true;
    };

    services.mbsync = {
      enable = true;
      frequency = "*:0/10";
      preExec = "${pkgs.notmuch-mailmover}/bin/notmuch-mailmover";
      postExec = "${pkgs.notmuch}/bin/notmuch new";
    };

    home.packages = [pkgs.notmuch.emacs pkgs.notmuch-mailmover];
    xdg.configFile."notmuch-mailmover/config.yaml".text = ''
      notmuch_config: ~/.config/notmuch/default/config
      maildir: ~/mails

      # Rename with mbsync
      rename: true
      rules:
        - folder: gmail/archive
          query: tag:personal and not tag:deleted and not tag:spam and not tag:inbox
        - folder: gmail/trash
          query: tag:personal and tag:deleted
        - folder: gmail/spam
          query: tag:personal and tag:spam
        - folder: gmail/inbox
          query: tag:personal and tag:inbox and not tag:deleted and not tag:spam
        - folder: unina/trash
          query: tag:university and tag:deleted
        - folder: unina/inbox
          query: tag:university and tag:inbox and not tag:deleted
    '';

    programs.notmuch = {
      enable = true;

      new = {
        tags = ["new"];
        ignore = [];
      };

      search = {
        excludeTags = ["deleted" "spam"];
      };

      maildir = {
        synchronizeFlags = true;
      };

      hooks = {
        # I let notmuch manage post-indexing stuff like this
        postNew = ''
          ${pkgs.afew}/bin/afew --verbose --tag --new

          SEARCH="tag:notify"
          NOTIFY_COUNT=$(${pkgs.notmuch}/bin/notmuch count "$SEARCH");

          if [ "$NOTIFY_COUNT" -gt 0 ]; then
            RESULTS=''$(${pkgs.notmuch}/bin/notmuch search --format=json --output=summary --limit=5 --sort="newest-first" "$SEARCH" | ${pkgs.jq}/bin/jq -r '.[] | "\(.authors): \(.subject)"')
            ${pkgs.libnotify}/bin/notify-send "$NOTIFY_COUNT New Emails:" "$RESULTS"
          fi

          ${pkgs.notmuch}/bin/notmuch tag -notify -- tag:notify
        '';
      };
    };

    programs.afew = {
      enable = true;
      # I'm not assigning a tag to `gmail/archive`. I just remove the tag `inbox` if I'm archiving something.
      # [TODO] FolderNameFilter.0 and FolderNameFilter.2 to improve.
      extraConfig = ''
        [ArchiveSentMailsFilter]
        sent_tag = sent
        [SpamFilter]
        spam_tag = spam

        [FolderNameFilter.0]
        folder_explicit_list = gmail/inbox gmail/archive gmail/drafts gmail/sent gmail/trash gmail/spam
        folder_transforms = gmail/inbox:personal gmail/archive:personal gmail/drafts:personal gmail/sent:personal gmail/trash:personal gmail/spam:personal
        folder_lowercases = true

        [FolderNameFilter.1]
        folder_explicit_list = gmail/inbox gmail/drafts gmail/sent gmail/trash gmail/spam
        folder_transforms = gmail/inbox:inbox gmail/drafts:draft gmail/sent:sent gmail/trash:deleted gmail/spam:spam
        folder_lowercases = true

        [FolderNameFilter.2]
        folder_explicit_list = unina/inbox unina/drafts unina/sent unina/trash
        folder_transforms = unina/inbox:university unina/drafts:university unina/sent:university unina/trash:university
        folder_lowercases = true

        [FolderNameFilter.3]
        folder_explicit_list = unina/inbox unina/drafts unina/sent unina/trash
        folder_transforms = unina/inbox:inbox unina/drafts:draft unina/sent:sent unina/trash:deleted
        folder_lowercases = true

        [Filter.0]
        message = Applying notify tag and removing new tag
        query = tag:unread and tag:new and tag:inbox
        tags = +notify;-new
      '';
    };

    accounts.email.accounts = {
      gmail = rec {
        primary = true;
        flavor = "gmail.com";
        realName = "Mario Liguori";
        address = "mario.liguori.056@gmail.com";
        userName = address;
        passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup gmailPrimary password";

        notmuch.enable = true;

        msmtp.enable = true;

        mbsync = {
          enable = true;
          subFolders = "Verbatim";

          groups.gmail = {
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
                    Sync = "All";
                    Expunge = "Both";
                  }
                  // channelExtraConfig;
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

        notmuch.enable = true;

        msmtp.enable = true;

        mbsync = {
          enable = true;
          subFolders = "Verbatim";

          groups.unina = {
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
                farPattern = "Posta inviata";
                nearPattern = "sent";
                extraConfig =
                  {
                    Sync = "All";
                    Expunge = "Both";
                  }
                  // channelExtraConfig;
              };

              drafts = {
                farPattern = "Bozze";
                nearPattern = "drafts";
                extraConfig =
                  {
                    Sync = "Pull";
                    Expunge = "Both";
                  }
                  // channelExtraConfig;
              };

              trash = {
                farPattern = "Posta eliminata";
                nearPattern = "trash";
                extraConfig =
                  {
                    Sync = "All";
                    Expunge = "None";
                  }
                  // channelExtraConfig;
              };
            };
          };
        };
      };
    };
  };
}
