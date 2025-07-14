;;; init-projects.el --- Projects management -*- lexical-binding: t -*-

;;; Commentary:

;; Git integration and projects' folders management.

;;; Code:

(setup envrc
  (:elpaca t)
  (:needs "direnv")
  (:with-hook after-init-hook
    (:hook envrc-global-mode)))

(setup magit
  (elpaca transient)
  (elpaca (magit :wait t))
  (:autoload magit-status)
  (:option magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;; (setup forge
;;   (:elpaca t)
;;   (:load-after magit))

(setup ediff
  (:autoload ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  (:option ediff-split-window-function 'split-window-horizontally
           ediff-window-setup-function 'ediff-setup-windows-plain))

(setup blamer
  (:elpaca t))

;; `projectile', not using to try `project.el'
(setup projectile
  (:disable t)
  (:elpaca t)
  (:hide-mode)

  ;; NOTE: Set this to the folder where you keep your Git repos!
  (:option projectile-project-search-path '("~/projects")
           projectile-switch-project-action #'projectile-dired)

  (projectile-mode)

  (:global "C-c C-p" projectile-command-map))

(setup consult-projectile
  (:disable t)
  (:elpaca t)
  (:load-after consult projectile))

;; `treemacs' stuff, I'm not using it
(setup treemacs
  (:disable t)
  (:elpaca t)
  (:option treemacs-deferred-git-apply-delay        0.5
           treemacs-directory-name-transformer      #'identity
           treemacs-display-in-side-window          t
           treemacs-eldoc-display                   'simple
           treemacs-file-event-delay                2000
           treemacs-file-follow-delay               0.2
           treemacs-file-name-transformer           #'identity
           treemacs-follow-after-init               t
           treemacs-expand-after-init               t
           treemacs-find-workspace-method           'find-for-file-or-pick-first
           treemacs-git-command-pipe                ""
           treemacs-goto-tag-strategy               'refetch-index
           treemacs-header-scroll-indicators        '(nil . "^^^^^^")
           treemacs-hide-dot-git-directory          t
           treemacs-indentation                     2
           treemacs-indentation-string              " "
           treemacs-is-never-other-window           t
           treemacs-max-git-entries                 5000
           treemacs-missing-project-action          'ask
           treemacs-move-forward-on-expand          nil
           treemacs-no-png-images                   nil
           treemacs-no-delete-other-windows         t
           treemacs-project-follow-cleanup          nil
           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
           treemacs-position                        'left
           treemacs-read-string-input               'from-child-frame
           treemacs-recenter-distance               0.1
           treemacs-recenter-after-file-follow      nil
           treemacs-recenter-after-tag-follow       nil
           treemacs-recenter-after-project-jump     'always
           treemacs-recenter-after-project-expand   'on-distance
           treemacs-litter-directories              '("/.direnv" "/node_modules" "/.venv" "/.cask")
           treemacs-project-follow-into-home        nil
           treemacs-show-cursor                     nil
           treemacs-show-hidden-files               t
           treemacs-silent-filewatch                nil
           treemacs-silent-refresh                  nil
           treemacs-sorting                         'alphabetic-asc
           treemacs-select-when-already-in-treemacs 'move-back
           treemacs-space-between-root-nodes        t
           treemacs-tag-follow-cleanup              t
           treemacs-tag-follow-delay                1.5
           treemacs-text-scale                      nil
           treemacs-user-mode-line-format           nil
           treemacs-user-header-line-format         nil
           treemacs-wide-toggle-width               70
           treemacs-width                           20
           treemacs-width-increment                 1
           treemacs-width-is-initially-locked       nil
           treemacs-workspace-switch-cleanup        nil)

  (:when-loaded
    (setq treemacs-collapse-dirs         (if treemacs-python-executable 3 0)
          treemacs-file-extension-regex  treemacs-last-period-regex-value)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-hide-gitignored-files-mode nil)

    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))


  (:global "M-0"        treemacs-select-window
           "C-c C-t 1"  treemacs-delete-other-windows
           "C-c C-t t"  treemacs
           "C-c C-t d"  treemacs-select-directory
           "C-c C-t b"  treemacs-bookmark
           "C-c C-t f"  treemacs-find-file
           "C-c C-t T"  treemacs-find-tag))

(setup treemacs-projectile
  (:disable t)
  (:elpaca t)
  (:load-after treemacs projectile))

(setup treemacs-all-the-icons
  (:disable t)
  (:elpaca t)
  (:with-after treemacs
    (treemacs-load-theme "all-the-icons")))

(setup treemacs-magit
  (:disable t)
  (:elpaca t)
  (:load-after treemacs magit))

(provide 'init-projects)
;;; init-projects.el ends here
