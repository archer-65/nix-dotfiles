;;; init-mail.el --- Mail configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Right now I'm using MU4E, I'm good with it, is used by many people, so finding snippets
;; is easy.  I could try Notmuch, however.

;;; Code:

;; Actual client for mails
(leaf mu4e
  :require t
  :commands mu4e mu4e-compose-new
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
	mu4e-compose-context-policy 'ask-if-none
	;; Why confirm quit?
	mu4e-confirm-quit nil)

  ;; User agent
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Use mu4e for sending e-mail
  (setq send-mail-function #'smtpmail-send-it
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

  (mu4e t))

;; Notifications!
(leaf mu4e-alert
  :doc "Enable notifications for mu4e"
  :straight t
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications)
  (setq mu4e-alert-notify-repeated-mails nil))

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
