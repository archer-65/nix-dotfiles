;;; init-projects.el --- Projects management -*- lexical-binding: t -*-

;;; Commentary:

;; Git integration and projects' folders management.

;;; Code:

(leaf treemacs
  :doc "Tree style directory visualization"
  :straight t
  :config
  (setq treemacs-width-is-initially-locked nil))

(leaf projectile
  :doc "Project management and navigation"
  :straight t
  :blackout t
  :config
  (projectile-mode)
  ;; :bind-keymap
  ;; ("C-c p"   . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(leaf magit
  :doc "Git interface"
  :straight t
  :commands magit-status
  :custom
  (magit-display-buffer-function . 'magit-display-buffer-same-window-except-diff-v1))

(leaf forge
  :after magit
  :straight t)

(provide 'init-projects)
;;; init-projects.el ends here
