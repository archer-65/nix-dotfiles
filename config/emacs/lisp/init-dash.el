;;; init-dash.el --- Dashboard configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration of my dashboard, loaded at startup.

;;; Code:

(leaf dashboard
  :straight t
  :blackout t
  :commands (all-the-icons-fileicon
	     all-the-icons-faicon
	     all-the-icons-octicon)
  :init
  ;; Basic UI settings
  (setq dashboard-banner-logo-title "SUCK(EMAC)S - Personal Workspace")
  (setq dashboard-startup-banner "~/.emacs.d/img/stallman.png")
  (setq dashboard-center-content t)
  ;; Icons
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)))
  (setq dashboard-heading-icons '((recents   . "history")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "briefcase")
                                  (registers . "database")))
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
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
  :hook
  (after-init-hook . dashboard-insert-startupify-lists)
  :config
  ;; Needed with PGTK/NativeComp
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))
  ;; (dashboard-refresh-buffer))

(provide 'init-dash)
;;; init-dash.el ends here
