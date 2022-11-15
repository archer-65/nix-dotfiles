;;; init-dired.el --- Dired -*- lexical-binding: t -*-

;;; Commentary:

;; Dired utilities and configuration for a better experience.

;;; Code:

(defun archer-dired-open-file ()
  "In Dired, open the file named on this line through xdg-open."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(leaf dired
  :commands (dired dired-jump)
  :bind
  ("C-x C-j" . dired-jump)
  ("C-c d" . dired-omit-mode)
  (:dired-mode-map
   ("C-c o" . archer-dired-open-file))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-omit-files "^\\.?#\\|^\\.$")
  (unless (version< emacs-version "29")
    (setopt dired-mouse-drag-files t))
  :custom
  (dired-listing-switches . "-agho --group-directories-first")
  :hook
  (dired-load-hook . dired-collapse)
  (dired-mode-hook . dired-omit-mode))

(leaf all-the-icons-dired
  :straight t
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(leaf trashed
  :doc "Visit system trash."
  :straight t
  :require t
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t)))

(provide 'init-dired)
;;; init-dired.el ends here
