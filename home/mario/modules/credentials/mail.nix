{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.credentials.mail-defaults;

  channelExtraConfig = {
    Create = "Near";
    SyncState = "*";
    # Read https://vxlabs.com/2021/03/21/mbsync-copyarrivaldate-yes/
    CopyArrivalDate = "yes";
  };

  lieerAccounts = filter (a: a.lieer.enable) (attrValues config.accounts.email.accounts);
  lieerSyncAccounts = filterAttrs (_: acc: acc.lieer.enable && acc.lieer.sync.enable) config.accounts.email.accounts;
in {
  options.mario.modules.credentials.mail-defaults = {
    enable = mkEnableOption "mail support";
  };

  config = mkIf cfg.enable {
    accounts.email.maildirBasePath = "${config.home.homeDirectory}/mails";
    accounts.email.accounts = {
      gmail = rec {
        primary = true;
        flavor = "gmail.com";
        realName = "Mario Liguori";
        address = "mario.liguori.056@gmail.com";
        userName = address;

        notmuch.enable = true;
        lieer = {
          enable = true;
          sync = {
            enable = true;
            frequency = "*:0/10";
          };
          settings = {
            replace_slash_with_dot = true;
            ignore_tags = ["new" "university"];
            ignore_remote_labels = [];
          };
        };
      };

      # "gmail.imap" = rec {
      #   primary = false;
      #   flavor = "gmail.com";
      #   realName = "Mario Liguori";
      #   address = "mario.liguori.056@gmail.com";
      #   userName = address;
      #   passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup gmailPrimary password";

      #   notmuch.enable = true;
      #   msmtp.enable = true;

      #   mbsync.enable = true;
      #   mbsync.subFolders = "Verbatim";

      #   mbsync.groups.gmail.channels = {
      #     inbox = {
      #       farPattern = "INBOX";
      #       nearPattern = "inbox";
      #       extraConfig =
      #         {
      #           Sync = "All";
      #           Expunge = "Both";
      #         }
      #         // channelExtraConfig;
      #     };

      #     sent = {
      #       farPattern = "[Gmail]/Sent Mail";
      #       nearPattern = "sent";
      #       extraConfig =
      #         {
      #           Sync = "All";
      #           Expunge = "Both";
      #         }
      #         // channelExtraConfig;
      #     };

      #     archive = {
      #       farPattern = "[Gmail]/All Mail";
      #       nearPattern = "archive";
      #       extraConfig =
      #         {
      #           Sync = "All";
      #           Expunge = "None";
      #         }
      #         // channelExtraConfig;
      #     };

      #     drafts = {
      #       farPattern = "[Gmail]/Drafts";
      #       nearPattern = "drafts";
      #       extraConfig =
      #         {
      #           Sync = "Pull";
      #           Expunge = "Both";
      #         }
      #         // channelExtraConfig;
      #     };

      #     trash = {
      #       farPattern = "[Gmail]/Trash";
      #       nearPattern = "trash";
      #       extraConfig =
      #         {
      #           Sync = "All";
      #           Expunge = "None";
      #         }
      #         // channelExtraConfig;
      #     };

      #     spam = {
      #       farPattern = "[Gmail]/Spam";
      #       nearPattern = "spam";
      #       extraConfig =
      #         {
      #           Sync = "All";
      #           Expunge = "Both";
      #         }
      #         // channelExtraConfig;
      #     };
      #   };
      # };

      # unina = rec {
      #   primary = false;
      #   realName = "Mario Liguori";
      #   userName = "mario.liguori6";
      #   address = "mario.liguori6@studenti.unina.it";
      #   passwordCommand = "${pkgs.libsecret}/bin/secret-tool lookup unina password";

      #   imap = {
      #     host = "studenti.unina.it";
      #     port = 993;
      #     tls.enable = true;
      #   };

      #   smtp = {
      #     host = "studenti.unina.it";
      #     port = 465;
      #     tls.enable = true;
      #   };

      #   notmuch.enable = true;
      #   msmtp.enable = true;

      #   mbsync.enable = true;
      #   mbsync.subFolders = "Verbatim";
      #   mbsync.groups.unina.channels = {
      #     inbox = {
      #       farPattern = "INBOX";
      #       nearPattern = "inbox";
      #       extraConfig =
      #         {
      #           Sync = "All";
      #           Expunge = "Both";
      #         }
      #         // channelExtraConfig;
      #     };

      #     sent = {
      #       farPattern = "Posta inviata";
      #       nearPattern = "sent";
      #       extraConfig =
      #         {
      #           Sync = "All";
      #           Expunge = "Both";
      #         }
      #         // channelExtraConfig;
      #     };

      #     drafts = {
      #       farPattern = "Bozze";
      #       nearPattern = "drafts";
      #       extraConfig =
      #         {
      #           Sync = "Pull";
      #           Expunge = "Both";
      #         }
      #         // channelExtraConfig;
      #     };

      #     trash = {
      #       farPattern = "Posta eliminata";
      #       nearPattern = "trash";
      #       extraConfig =
      #         {
      #           Sync = "All";
      #           Expunge = "None";
      #         }
      #         // channelExtraConfig;
      #     };
      #   };
      # };
    };

    programs.notmuch = {
      enable = true;

      new = {
        tags = ["new"];
        ignore = [];
      };

      search = {
        excludeTags = ["deleted" "spam"];
      };

      maildir.synchronizeFlags = true;

      hooks = let
        tag-and-notify = ''
          ${pkgs.afew}/bin/afew --verbose --tag --new

          SEARCH="tag:notify"
          NOTIFY_COUNT=$(${pkgs.notmuch}/bin/notmuch count "$SEARCH");

          if [ "$NOTIFY_COUNT" -gt 0 ]; then
            RESULTS=''$(${pkgs.notmuch}/bin/notmuch search --format=json --output=summary --limit=5 --sort="newest-first" "$SEARCH" | ${pkgs.jq}/bin/jq -r '.[] | "\(.authors): \(.subject)"')
            ${pkgs.libnotify}/bin/notify-send --icon=mail-unread-symbolic "$NOTIFY_COUNT New Emails:" "$RESULTS"
          fi

          ${pkgs.notmuch}/bin/notmuch tag -notify -- tag:notify
        '';
      in {
        # I let notmuch manage post-indexing stuff like this
        postNew = tag-and-notify;
      };
    };

    programs = {
      mbsync.enable = true;
      msmtp.enable = true;
    };

    programs.afew = {
      enable = true;
      # I'm not assigning a tag to `gmail/archive`. I just remove the tag `inbox` if I'm archiving something.
      extraConfig = ''
        [ArchiveSentMailsFilter]
        sent_tag = sent
        [SpamFilter]
        spam_tag = spam

        # With `lieer`
        [FolderNameFilter.0]
        folder_explicit_list = gmail/mail
        folder_transforms = gmail/mail:personal
        folder_lowercases = true

        # With `imap`
        # [FolderNameFilter.0]
        # folder_explicit_list = gmail/inbox gmail/archive gmail/drafts gmail/sent gmail/trash gmail/spam
        # folder_transforms = gmail/inbox:personal gmail/archive:personal gmail/drafts:personal gmail/sent:personal gmail/trash:personal gmail/spam:personal
        # folder_lowercases = true

        # [FolderNameFilter.1]
        # folder_explicit_list = gmail/inbox gmail/drafts gmail/sent gmail/trash gmail/spam
        # folder_transforms = gmail/inbox:inbox gmail/drafts:draft gmail/sent:sent gmail/trash:deleted gmail/spam:spam
        # folder_lowercases = true

        # [FolderNameFilter.2]
        # folder_explicit_list = unina/inbox unina/drafts unina/sent unina/trash
        # folder_transforms = unina/inbox:university unina/drafts:university unina/sent:university unina/trash:university
        # folder_lowercases = true

        # [FolderNameFilter.3]
        # folder_explicit_list = unina/inbox unina/drafts unina/sent unina/trash
        # folder_transforms = unina/inbox:inbox unina/drafts:draft unina/sent:sent unina/trash:deleted
        # folder_lowercases = true

        [Filter.0]
        message = Applying notify tag and removing new tag
        query = tag:unread and tag:new and tag:inbox
        tags = +notify;-new

        [Filter.1]
        message = Removing new tag
        query = tag:new
        tags = -new
      '';
    };

    home.packages = [pkgs.notmuch-mailmover];
    xdg.configFile."notmuch-mailmover/config.yaml".text = ''
      notmuch_config: ${config.home.sessionVariables.NOTMUCH_CONFIG}
      maildir: ${config.accounts.email.maildirBasePath}

      # Rename with mbsync
      rename: true
      rules:
        # gmail imap
        # - folder: gmail/archive
        #   query: tag:personal and not tag:deleted and not tag:spam and not tag:inbox
        # - folder: gmail/trash
        #   query: tag:personal and tag:deleted
        # - folder: gmail/spam
        #   query: tag:personal and tag:spam
        # - folder: gmail/inbox
        #   query: tag:personal and tag:inbox and not tag:deleted and not tag:spam
        # - folder: unina/trash
        #   query: tag:university and tag:deleted
        # - folder: unina/inbox
        #   query: tag:university and tag:inbox and not tag:deleted
    '';

    services.mbsync = {
      enable = true;
      frequency = "*:0/10";
      preExec = "${pkgs.notmuch-mailmover}/bin/notmuch-mailmover";
      postExec = "${pkgs.notmuch}/bin/notmuch new";
    };

    services.lieer.enable = true;
    systemd.user.services = mkIf config.services.lieer.enable (
      mapAttrs' (_: acc:
        nameValuePair "lieer-${acc.name}" {
          Service.ExecStartPost = "${pkgs.notmuch}/bin/notmuch new";
        })
      lieerSyncAccounts
    );

    home.activation = mkIf (lieerAccounts != []) {
      createLieerMaildir = lib.hm.dag.entryBetween ["linkGeneration"] ["writeBoundary"] ''
        $DRY_RUN_CMD mkdir -m700 -p $VERBOSE_ARG ${
          lib.concatMapStringsSep " "
          (a: "${a.maildir.absPath}/mail/{cur,new,tmp}")
          lieerAccounts
        }
      '';
    };
  };
}
