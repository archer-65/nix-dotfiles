;;; init-dired.el --- Dired -*- lexical-binding: t -*-

;;; Commentary:

;; Dired utilities and configuration for a better experience.

;;; Code:
(setup dired
  ;; 'Kay, with this I'm good, maybe
  (defun archer-dired-open-file ()
    "In Dired, open the file named on this line through xdg-open."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))

  ;; Kill the current Dired buffer, then visit the file or directory
  (put 'dired-find-alternate-file 'disabled nil)

  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq dired-use-ls-dired t
              insert-directory-program gls))))

  ;; Emacs 29 options
  (unless (version< emacs-version "29")
    (setopt dired-mouse-drag-files t
            dired-make-directory-clickable t
            dired-free-space nil))

  (:option dired-listing-switches "-agho --group-directories-first"
           dired-kill-when-opening-new-dired-buffer t
           dired-recursive-copies 'always
           dired-recursive-deletes 'always
           dired-auto-revert-buffer #'dired-directory-changed-p
           dired-dwim-target t
           dired-hide-details-hide-symlink-targets nil
           delete-by-moving-to-trash t)

  (:with-map dired-jump-map
    (:bind "j" dired-jump))

  (:with-map dired-mode-map
    (:bind "M-<return>" archer-dired-open-file)))

(setup (:require dired-x)
  (:option dired-clean-confirm-killing-deleted-buffers t
           dired-clean-up-buffers-too t
           dired-x-hands-off-my-keys t
           dired-omit-files "^\\.$\\|^\\.[^.]")

  (:with-map dired-mode-map
    (:bind "C-c d" dired-omit-mode))

  (:with-map dired-mode-map
    (:bind "I" #'dired-info))

  (:with-mode dired-mode
    (:hook dired-omit-mode)))

(setup (:require dired-aux)
  (:option dired-create-destination-dirs 'always
           dired-do-revert-buffer t
           dired-isearch-filenames 'dwim
           dired-vc-rename-file t))

(setup (:require wdired)
  (:option wdired-allow-to-change-permissions t
           wdired-create-parent-directories t))

(setup (:require image-dired)
  (:option image-dired-external-viewer "xdg-open"
           image-dired-thumb-size 80
           image-dired-thumb-margin 2
           image-dired-thumb-relief 0
           image-dired-thumbs-per-row 4)

  (:with-map image-dired-thumbnail-mode-map
    (:bind "<return>" #'image-dired-thumbnail-display-external)))

(elpaca-setup diredfl
  :disabled
  (diredfl-global-mode 1))

(elpaca-setup dired-subtree
  (:load-after dired)
  (:option dired-subtree-use-backgrounds nil)
  (:with-map dired-mode-map
    (:bind
      "<tab>" dired-subtree-toggle
      "<backtab>" dired-subtree-remove)))

(elpaca-setup dired-sidebar
  (:load-after dired)
  (:global "C-x C-n" dired-sidebar-toggle-sidebar))

(elpaca-setup dired-collapse
  (:with-after dired
    (:hook-into dired-mode-hook)))

(elpaca-setup all-the-icons-dired
  (:option all-the-icons-dired-monochrome nil)
  (:with-after (all-the-icons dired)
    (:hook-into dired-mode-hook)))

(elpaca-setup trashed
  (:option trashed-action-confirmer 'y-or-n-p
           trashed-use-header-line t
           trashed-sort-key '("Date deleted" . t)))

(provide 'init-dired)
;;; init-dired.el ends here
