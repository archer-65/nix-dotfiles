;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(leaf cmake-mode
  :straight t
  :mode "\\CMakeLists\\.txt\\'" "\\.cmake\\'")

(leaf nix-mode
  :straight t
  :mode "\\.nix\\'")

(leaf yaml-mode
  :straight t
  :mode "\\.yml\\'")

(leaf json-mode
  :straight t
  :mode "\\.json\\'")

(leaf c-mode
  :mode "\\.c\\'")

(leaf rustic
  :straight t
  :mode "\\.rs\\'"
  :config
  (setq rustic-format-on-save nil) ;; There's `format-all-mode'
  (setq rustic-lsp-client archer-lsp-client))

(leaf terraform-mode
  :straight t
  :mode "\\.tf\\'")

(leaf company-terraform
  :straight t
  :after terraform-mode
  :config
  (company-terraform-init)
  (defun archer-cape-company-terraform()
    "Add completion at point functions made from company backends for `terraform'."
    (setq-local
     completion-at-point-functions
     (append (list (cape-company-to-capf #'company-terraform)) completion-at-point-functions)))
  (add-hook 'terraform-mode-hook 'archer-cape-company-terraform))

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
