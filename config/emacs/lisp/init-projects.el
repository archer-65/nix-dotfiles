;;; init-projects.el --- Projects management -*- lexical-binding: t -*-

;;; Commentary:

;; Git integration and projects' folders management.

;;; Code:

(setup (:pkg projectile)
  (:blackout)

  ;; NOTE: Set this to the folder where you keep your Git repos!
  (:option projectile-project-search-path '("~/projects")
           projectile-switch-project-action #'projectile-dired)

  (projectile-mode)

  (:global "C-c C-p" projectile-command-map))

(setup (:pkg direnv)
  (:hook-into prog-mode))

(setup (:pkg magit)
  (:autoload magit-status)
  (:option magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(setup (:pkg forge)
  (:load-after magit))

(provide 'init-projects)
;;; init-projects.el ends here
