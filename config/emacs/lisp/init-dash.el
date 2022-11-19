;;; init-dash.el --- Dashboard configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration of my dashboard, loaded at startup.

;;; Code:

(setup (:pkg dashboard)
  (:option
   ;; Banner
   dashboard-banner-logo-title "SUCK(EMAC)S - Personal Workspace"
   dashboard-startup-banner (expand-file-name "img/stallman.png" user-emacs-directory)
   dashboard-center-content t
   ;; Icons
   dashboard-set-heading-icons t
   dashboard-set-file-icons t
   dashboard-items '((recents . 5)
                     (bookmarks . 5))

   ;; Headings
   dashboard-heading-icons '((recents   . "history")
                             (bookmarks . "bookmark")
                             (agenda    . "calendar")
                             (projects  . "briefcase")
                             (registers . "database"))

   ;; Navigator under banner
   dashboard-set-navigator t
   dashboard-navigator-buttons
   `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
       "Homepage"
       "Browse homepage"
       (lambda (&rest _) (browse-url "https://github.com/archer-65/emacs-config")))
      (,(all-the-icons-faicon "archive" :height 1.1 :v-adjust 0.0)
       "Update Packages"
       "Click to updates your packages"
       (lambda (&rest _) (straight-pull-all)))
      (,(all-the-icons-octicon "gear" :height 1.1 :v-adjust 0.0)
       "Configuration"
       "Click to config Emacs"
       (lambda (&rest _) (find-file "~/.dotfiles/config/emacs/Emacs.org"))))))

  (dashboard-setup-startup-hook)
  ;; This is required with PGTK!
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  (:with-hook after-init-hook
    (:hook dashboard-insert-startupify-lists)))

(provide 'init-dash)
;;; init-dash.el ends here
