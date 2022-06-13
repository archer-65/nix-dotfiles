;;; init-projects.el --- Projects management -*- lexical-binding: t -*-

;;; Commentary:

;; Git integration and projects' folders management.

;;; Code:

(leaf treemacs
  :doc "Tree style directory visualization"
  :ensure t
  :config
  (setq treemacs-width-is-initially-locked nil))

(leaf projectile
  :doc "Project management and navigation"
  :ensure t
  :blackout t
  :config (projectile-mode)
  :bind-keymap
  ("C-c p"   . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(leaf magit
  :doc "Git interface"
  :ensure t
  :commands magit-status
  :custom
  (magit-display-buffer-function . 'magit-display-buffer-same-window-except-diff-v1))

(leaf forge
  :after magit)

(provide 'init-projects)
;;; init-projects.el ends here
