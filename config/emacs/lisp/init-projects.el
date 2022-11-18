;;; init-projects.el --- Projects management -*- lexical-binding: t -*-

;;; Commentary:

;; Git integration and projects' folders management.

;;; Code:

(setup (:straight projectile)
  (:doc "Project management and navigation")
  (:blackout)

  ;; NOTE: Set this to the folder where you keep your Git repos!
  (:option projectile-project-search-path '("~/projects")
	   projectile-switch-project-action #'projectile-dired)

  (projectile-mode)

  (:global "C-c C-p" projectile-command-map))

(setup (:straight magit)
  (:doc "Git interface")
  (:autoload magit-status)
  (:option magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(setup (:straight forge)
  (:load-after magit))

(provide 'init-projects)
;;; init-projects.el ends here
