;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(leaf nix-mode
  :straight t
  :mode "\\.nix\\'")

(leaf yaml-mode
  :straight t
  :mode "\\.yml\\'")

(leaf json-mode
  :straight t
  :mode "\\.json\\'")

(leaf rustic
  :straight t
  :mode "\\.rs\\'"
  :bind (:rustic-mode-map
	 ("M-j" . lsp-ui-imenu)
         ("M-?" . lsp-find-references)
         ("C-c C-c l" . flycheck-list-errors)
         ("C-c C-c a" . lsp-execute-code-action)
         ("C-c C-c r" . lsp-rename)
         ("C-c C-c q" . lsp-workspace-restart)
         ("C-c C-c Q" . lsp-workspace-shutdown)
         ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t))

(leaf terraform-mode
  :straight t
  :mode "\\.tf\\'"
  :config
  (leaf company-terraform
    :straight t
    :config
    (company-terraform-init)))

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
