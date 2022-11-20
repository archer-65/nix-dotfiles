;;; init-dired.el --- Dired -*- lexical-binding: t -*-

;;; Commentary:

;; Dired utilities and configuration for a better experience.

;;; Code:

(defun archer-dired-open-file ()
  "In Dired, open the file named on this line through xdg-open."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(setup dired
  ;; Kill the current Dired buffer, then visit the file or directory
  (put 'dired-find-alternate-file 'disabled nil)

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

  (:global "C-x C-j" dired-jump)
  (:bind "C-c o" archer-dired-open-file))

(setup (:require dired-x)
  (:option dired-clean-confirm-killing-deleted-buffers nil
     dired-clean-up-buffers-too t
     dired-x-hands-off-my-keys t
     dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'")

  (:global "C-c d" dired-omit-mode)
  (:bind-into dired-mode-map
    "I" #'dired-info)

  (:hooks dired-mode-hook dired-omit-mode))

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

  (:bind-into image-dired-thumbnail-mode-map
    "<return>" #'image-dired-thumbnail-display-external))

(setup (:pkg dired-collapse)
  (:load-after dired
    (:hooks dired-load-hook dired-collapse)))

(setup (:pkg all-the-icons-dired)
  (:load-after (all-the-icons dired)
    (:hooks dired-mode-hook all-the-icons-dired-mode)))

(setup (:pkg trashed)
  (:doc "Visit system trash.")
  (:option trashed-action-confirmer 'y-or-n-p
     trashed-use-header-line t
     trashed-sort-key '("Date deleted" . t)))

(provide 'init-dired)
;;; init-dired.el ends here
