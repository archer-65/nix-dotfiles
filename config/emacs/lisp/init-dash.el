;;; init-dash.el --- Dashboard configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration of my dashboard, loaded at startup.

;;; Code:

(leaf dashboard
  :ensure t
  ;; In server mode is needed, some symbols are seen as void
  :after all-the-icons
  :config
  (dashboard-setup-startup-hook)
  ;; Needed with PGTK/NativeComp
  (dashboard-refresh-buffer)
  (setq initial-buffer-choice(lambda nil (get-buffer "*dashboard*")))
  ;; Items
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)))
  (setq dashboard-set-haeding-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner "~/.emacs.d/img/stallman.png")
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  ;; Buttons
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/archer-65/emacs-config")))
           (,(all-the-icons-faicon "archive" :height 1.1 :v-adjust 0.0)
            "Update Packages"
            "Click to updates your packages"
            (lambda (&rest _) (auto-package-update-now)))
           (,(all-the-icons-octicon "gear" :height 1.1 :v-adjust 0.0)
            "Configuration"
            "Click to config Emacs"
            (lambda (&rest _) (find-file "~/.dotfiles/config/emacs/Emacs.org")))))))

(provide 'init-dash)
;;; init-dash.el ends here
