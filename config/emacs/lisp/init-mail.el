;;; init-mail.el --- Mail configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Right now I'm using MU4E, I'm good with it, is used by many people, so finding snippets
;; is easy.  I could try Notmuch, however.

;;; Code:

(defgroup archer:notmuch()
  "Extensions for notmuch"
  :group 'notmuch)

(defcustom archer:notmuch-delete-tag "deleted"
  "Tag that applies to mail marked for deletion"
  :type 'string
  :group 'archer:notmuch)

(defcustom archer:notmuch-mark-delete-tags
  `(,(format "+%s" archer:notmuch-delete-tag) "-inbox" "-archived" "-unread")
  "List of tags to mark for deletion."
  :type '(repeat string)
  :group 'archer:notmuch)

(defcustom archer:notmuch-mark-archive-tags '( "+archived" "-deleted" "-inbox" "-unread")
  "List of tags to mark for archive."
  :type '(repeat string)
  :group 'archer:notmuch)

(defcustom archer:notmuch-mark-flag-tags '("+flagged" "-unread")
  "List of tags to mark as important (flagged is a special tag)"
  :type '(repeat string)
  :group 'archer:notmuch)

(defcustom archer:notmuch-mark-spam-tags '("+spam" "-inbox" "-unread")
  "List of tags to mark as spam."
  :type '(repeat string)
  :group 'archer:notmuch)

;; Actual client for mails
(leaf notmuch
  :load-path "~/.nix-profile/share/emacs/site-lisp"
  :commands notmuch
  :config
  ;; UI
  (setopt notmuch-show-logo t
	  notmuch-column-control 0.5
	  notmuch-hello-auto-refresh t
	  notmuch-hello-recent-searches-max 15
	  notmuch-hello-thousands-separator "."
	  notmuch-show-all-tags-list t
	  notmuch-hello-sections '(notmuch-hello-insert-header
				   notmuch-hello-insert-saved-searches
				   notmuch-hello-insert-search
				   notmuch-hello-insert-recent-searches
				   notmuch-hello-insert-alltags
				   notmuch-hello-insert-footer))
  ;; Search
  (setopt notmuch-search-oldest-first nil
	  notmuch-show-empty-saved-searches t
	  notmuch-search-result-format
	  '(("date" . "%12s ")
	    ("count" . "%-7s ")
	    ("authors" . "%-20s ")
	    ("subject" . "%80s ")
	    ("tags" . "[%s]"))
	  notmuch-tree-result-format
	  '(("date" . "%12s  ")
	    ("authors" . "%-20s")
	    ((("tree" . "%s")
	      ("subject" . "%s"))
	     . " %-80s ")
	    ("tags" . "[%s]"))
	  notmuch-search-line-faces
	  '(("unread" . notmuch-search-unread-face)
	    ("flagged" . notmuch-search-flagged-face)))

  ;; Saved searches
  (setopt notmuch-saved-searches
	  ;; Personal
	  `(( :name "📥 inbox (personal)"
	      :query "tag:inbox and tag:personal not tag:archived"
	      :sort-order newest-first
	      :key ,(kbd "p i"))
	    ( :name "📔 unread (personal)"
              :query "tag:unread and tag:inbox and tag:personal not tag:archived"
              :sort-order newest-first
              :key ,(kbd "p u"))
	    ;; University
	    ( :name "📥 inbox (university)"
	      :query "tag:inbox and tag:university"
	      :sort-order newest-first
	      :key ,(kbd "u i"))
	    ( :name "📔 unread (university)"
              :query "tag:unread and tag:inbox and tag:university"
              :sort-order newest-first
              :key ,(kbd "u u"))))

  ;; Tags
  (setopt notmuch-archive-tags archer:notmuch-mark-archive-tags
	  notmuch-message-replied-tags '("+replied")
	  notmuch-message-forwarded-tags '("+forwarded")
	  notmuch-show-mark-read-tags '("-unread")
	  notmuch-draft-tags '("+draft")
	  notmuch-draft-folder "drafts"
	  notmuch-draft-save-plaintext 'ask)

  ;; Tag formats (with emojis)
  (setq notmuch-tag-formats
	'(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flagged" (propertize tag 'face 'notmuch-tag-flagged) ;; Icon is enough
           (concat "🚩")))
        notmuch-tag-deleted-formats
        '(("unread" (notmuch-apply-face bare-tag 'notmuch-tag-deleted)
           (concat "🚫" tag))
          (".*" (notmuch-apply-face tag 'notmuch-tag-deleted)
           (concat "🚫" tag)))
        notmuch-tag-added-formats
        '((".*" (notmuch-apply-face tag 'notmuch-tag-added)
           (concat "✏️" tag))))

  ;; Reading
  (setopt notmuch-show-relative-dates t
          notmuch-show-all-multipart/alternative-parts nil
          notmuch-show-indent-messages-width 1
          notmuch-show-indent-multipart t
          notmuch-show-part-button-default-action 'notmuch-show-view-part
          notmuch-show-text/html-blocked-images "." ; block everything
          notmuch-wash-wrap-lines-length 120
          notmuch-unthreaded-show-out nil
          notmuch-message-headers '("To" "Cc" "Subject" "Date")
          notmuch-message-headers-visible t)

  (setopt notmuch-wash-citation-lines-prefix 3
          notmuch-wash-citation-lines-suffix 3)

  ;; TODO Composition

  ;; Hooks

  ;; Bindings
  )

(leaf mu4e
  :require t
  :disabled t
  :commands mu4e mu4e-compose-new
  :load-path "~/.nix-profile/share/emacs/site-lisp/mu4e"
  :init
  (provide 'html2text)
  :config
  ;; Load org-mode integration
  (require 'org-mu4e)
  (require 'mu4e-contrib)

  ;; General
  (setq mu4e-get-mail-command "mbsync -a"
	;; Update every 5 minutes
	mu4e-update-interval (* 5 60)
	;; Images
	mu4e-view-show-images t
	mu4e-view-image-max-width 800
	;; Don't keep message buffers around
	message-kill-buffer-on-exit t
	;; Start with default context
	mu4e-context-policy 'pick-first
	mu4e-compose-context-policy 'ask
	;; Why confirm quit?
	mu4e-confirm-quit nil)

  ;; User agent
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Use mu4e for sending e-mail
  (setq send-mail-function 'smtpmail-send-it
	message-sendmail-f-is-evil t
	message-sendmail-extra-arguments '("--read-envelope-from")
	message-send-mail-function 'message-send-mail-with-sendmail)

  ;; Mail settings
  (setq mu4e-maildir "~/mails"
	;; This is set to 't' to avoid mail syncing issues when using mbsync
	mu4e-change-filenames-when-moving t
	mu4e-main-buffer-hide-personal-addresses t)

  (setq mu4e-contexts
        `(
	  ;; Gmail Primary (new) Account
          ,(make-mu4e-context
            :name "Gmail"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "mario.liguori.056@gmail.com")
                    (smtpmail-smtp-user . "mario.liguori.056@gmail.com")
                    (user-full-name     . "Mario Liguori")
                    (mu4e-sent-folder   . "/gmail/sent")
                    (mu4e-drafts-folder . "/gmail/drafts")
                    (mu4e-trash-folder  . "/gmail/trash")
                    (mu4e-maildir-shortcuts .
                                            (("/gmail/inbox"  . ?i)
                                             ("/gmail/sent"   . ?s)
                                             ("/gmail/trash"  . ?t)
                                             ("/gmail/drafts" . ?d)))))
          ;;Unina
          ,(make-mu4e-context
            :name "Unina"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/unina" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "mario.liguori6@studenti.unina.it")
                    (smtpmail-smtp-user . "mario.liguori6@studenti.unina.it")
                    (user-full-name     . "Mario Liguori")
                    (mu4e-sent-folder   . "/unina/sent")
                    (mu4e-drafts-folder . "/unina/drafts")
                    (mu4e-trash-folder  . "/unina/trash")
                    (mu4e-maildir-shortcuts .
                                            (("/unina/inbox"  . ?i)
                                             ("/unina/sent"   . ?s)
                                             ("/unina/trash"  . ?t)
                                             ("/unina/drafts" . ?d)))))))
  ;; Set Bookmarks for all
  (setq  mu4e-bookmarks '(( :name  "Unread messages"
                            :query "flag:unread AND NOT flag:trashed"
                            :key ?u)
                          ( :name "Today's messages"
                            :query "date:today..now"
                            :key ?t)))

  (mu4e t))

;; Notifications!
(leaf mu4e-alert
  :doc "Enable notifications for mu4e"
  :straight t
  :disabled t
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications)
  (setq mu4e-alert-notify-repeated-mails nil))

;; Org enhanced messages
(leaf org-msg
  :straight t
  :disabled t
  :after (mu4e org)
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-default-alternatives '((new		. (text html))
                                       (reply-to-html	. (text html))
                                       (reply-to-text	. (text)))
        org-msg-convert-citation t)
  (org-msg-mode))

(provide 'init-mail)
;;; init-mail.el ends here
