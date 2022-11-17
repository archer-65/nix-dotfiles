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
  (:autoload dired dired-jump)

  (unless (version< emacs-version "29")
    (setopt dired-mouse-drag-files t))

  (put 'dired-find-alternate-file 'disabled nil)

  (:option dired-listing-switches "-agho --group-directories-first"
	   dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'"
	   dired-kill-when-opening-new-dired-buffer t)

  (:global "C-x C-j" dired-jump
	   "C-c d"   dired-omit-mode)

  (:bind "C-c o" archer-dired-open-file)

  (:hooks dired-load-hook dired-collapse
	  dired-mode-hook dired-omit-mode))

(setup (:straight all-the-icons-dired)
  (:hook-into dired-mode-hook))

(setup (:straight trashed)
  :doc "Visit system trash."
  (:option trashed-action-confirmer 'y-or-n-p
	   trashed-use-header-line t
	   trashed-sort-key '("Date deleted" . t)))

(provide 'init-dired)
;;; init-dired.el ends here
