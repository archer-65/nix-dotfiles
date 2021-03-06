;;; init-lsp.el --- Language Server Protocol client -*- lexical-binding: t -*-

;;; Commentary:

;; Here the configuration for LSP-Mode/Eglot.

;;; Code:

(leaf lsp-mode
  :commands lsp
  :straight t
  :bind
  (lsp-mode-map
    ("<tab>" . company-indent-or-complete-common))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
					:major-modes '(nix-mode)
					:server-id 'nix))
  :hook
  (c-mode-hook    . lsp)
  (c++-mode-hook  . lsp)
  (java-mode-hook . lsp)
  (nix-mode-hook  . lsp)
  (lsp-mode-hook  . lsp-enable-which-key-integration))

(leaf lsp-ui
  :straight t
  :after lsp
  :commands lsp-ui-mode)

(leaf lsp-treemacs
  :straight t
  :after lsp
  :commands lsp-treemacs-errors-list)

(leaf lsp-java
  :straight t
  :after lsp
  :require t
  :hook
  (java-mode-hook . lsp))

;; optionally if you want to use debugger
;; (leaf dap-mode)
;; (leaf dap-LANGUAGE) to load the dap adapter for your language

(provide 'init-lsp)
;;; init-lsp.el ends here
