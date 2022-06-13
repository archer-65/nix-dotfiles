;;; init-mail.el --- Mail configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Right now I'm using MU4E, I'm good with it, is used by many people, so finding snippets
;; is easy.  I could try Notmuch, however.

;;; Code:

;; Actual client for mails
(leaf mu4e
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :require t
  :config

  ;; Use mu4e for sending e-mail
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Needed? I don't know
  ;; (setq sendmail-program "/usr/bin/msmtp")
  (setq message-sendmail-f-is-evil t)
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq send-mail-function 'smtpmail-send-it)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  ;; (setq mu4e-compose-signature "Sent from Emacs")

  ;; (with-eval-after-load "emojify"
  ;;   (delete 'mu4e-headers-mode emojify-inhibit-major-modes))

  ;; Contrib
  (require 'mu4e-contrib)

  (setq shr-color-visible-luminance-min 60)
  (setq shr-color-visible-distance-min 5)
  (setq shr-use-colors nil)
  (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))

  ;; Load org-mode integration
  (require 'org-mu4e)

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-maildir "~/mails")
  (setq mu4e-main-buffer-hide-personal-addresses t)

  (setq mu4e-contexts
        `(
          ;; Gmail Account
          ,(make-mu4e-context
            :name "Gmail"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
            :vars '((user-email-address . "mariogt2009@gmail.com")
                    (smtpmail-smtp-user . "mariogt2009@gmail.com")
                    (user-full-name     . "Mario Liguori")
                    (mu4e-sent-folder   . "/Gmail/[Gmail]/Sent Mail")
                    (mu4e-drafts-folder . "/Gmail/[Gmail]/Drafts")
                    (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")
                    (mu4e-maildir-shortcuts .
                                            (("/Gmail/Inbox"     . ?i)
                                             ("/Gmail/[Gmail]/Sent Mail" . ?s)
                                             ("/Gmail/[Gmail]/Trash"     . ?t)
                                             ("/Gmail/[Gmail]/Drafts"    . ?d)))))
          ;; Outlook
          ,(make-mu4e-context
            :name "Outlook"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Outlook" (mu4e-message-field msg :maildir))))
            :vars '((user-email-address . "mariogt2009@live.it")
                    (smtpmail-smtp-user . "mariogt2009@live.it")
                    (user-full-name     . "Mario Liguori")
                    (mu4e-drafts-folder . "/Outlook/Drafts")
                    (mu4e-sent-folder   . "/Outlook/Sent")
                    (mu4e-refile-folder . "/Outlook/Archive")
                    (mu4e-trash-folder  . "/Outlook/Deleted")
                    (mu4e-maildir-shortcuts .
                                            (("/Outlook/Inbox"   . ?i)
                                             ("/Outlook/Sent"    . ?s)
                                             ("/Outlook/Deleted" . ?t)
                                             ("/Outlook/Drafts"  . ?d)))))

          ;;UniNa
          ,(make-mu4e-context
            :name "Unina"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Unina" (mu4e-message-field msg :maildir))))
            :vars '((user-email-address . "mario.liguori6@studenti.unina.it")
                    (smtpmail-smtp-user . "mario.liguori6@studenti.unina.it")
                    (user-full-name     . "Mario Liguori")
                    (mu4e-drafts-folder . "/Unina/Bozze")
                    (mu4e-sent-folder   . "/Unina/Posta inviata")
                    (mu4e-trash-folder  . "/Unina/Deleted Items")
                    (mu4e-maildir-shortcuts .
                                            (("/Unina/Inbox"         . ?i)
                                             ("/Unina/Posta inviata" . ?s)
                                             ("/Unina/Deleted Items" . ?t)
                                             ("/Unina/Bozze"         . ?d)))))))
  ;; Set Bookmarks for all
  (setq  mu4e-bookmarks '(( :name  "Unread messages"
                            :query "flag:unread AND NOT flag:trashed"
                            :key ?u)
                          ( :name "Today's messages"
                            :query "date:today..now"
                            :key ?t)))

  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-policy 'pick-first)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Don't ask to quit... why is this the default?
  (setq mu4e-confirm-quit nil)
  (mu4e t))

;; Notifications!
(leaf mu4e-alert
  :doc "Enable notifications for mu4e"
  :ensure t
  :after mu4e
  :init
  (mu4e-alert-set-default-style 'libnotify)
  ;; (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

  ;; Really, I don't know if this is needed anymore, I'll try the forked package
  ;; with the pull request sent by me.
  (defun mu4e-alert--get-mu4e-frame ()
    "Try getting a frame containing a mu4e buffer."
    (car (delq nil (mapcar (lambda (buffer)
                             (when (and buffer
                                        (get-buffer-window buffer t))
                               (window-frame (get-buffer-window buffer t))))
                           (list mu4e-main-buffer-name)))))

  (defun mu4e-alert-filter-repeated-mails (mails)
    "Filters the MAILS that have been seen already."
    (cl-remove-if (lambda (mail)
                    (prog1 (and (not mu4e-alert-notify-repeated-mails)
                                (ht-get mu4e-alert-repeated-mails
                                        (plist-get mail :message-id)))
                      (ht-set! mu4e-alert-repeated-mails
                               (plist-get mail :message-id)
                               t)))
                  mails))

  (setq mu4e-alert-notify-repeated-mails nil)
  :hook
  (after-init-hook . mu4e-alert-enable-notifications))

;; Org enhanced messages
(leaf org-msg
  :ensure t
  :after mu4e
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
