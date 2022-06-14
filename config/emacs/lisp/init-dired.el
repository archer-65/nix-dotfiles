;;; init-dired.el --- Dired -*- lexical-binding: t -*-

;;; Commentary:

;; Dired utilities and configuration for a better experience.

;;; Code:

(defun archer/dired-open-file ()
  "In Dired, open the file named on this line through xdg-open."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(leaf dired
  :ensure nil
  :commands (dired dired-jump)
  :bind
  ("C-x C-j" . dired-jump)
  (:dired-mode-map
   ("C-c o" . archer/dired-open-file))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setopt dired-mouse-drag-files t)
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "<return>")
                          'dired-find-alternate-file)
              (define-key dired-mode-map (kbd "^")
                          (lambda () (interactive) (find-alternate-file "..")))))
  :custom
  (dired-listing-switches . "-agho --group-directories-first")
  :hook
  (dired-load-hook . (lambda () (interactive) (dired-collapse))))

(leaf dired-single
  :commands (dired dired-jump))

(leaf all-the-icons-dired
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(leaf dired-hide-dotfiles
  :hook (dired-mode-hook . dired-hide-dotfiles-mode)
  :config
  (define-key dired-mode-map (kbd "C-c d") 'dired-hide-dotfiles-mode))

(leaf trashed
  :doc "Visit system trash."
  :ensure t
  :require t
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t)))

(provide 'init-dired)
;;; init-dired.el ends here
