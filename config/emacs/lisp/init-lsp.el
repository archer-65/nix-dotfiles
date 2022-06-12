;;; init-lsp.el --- Language Server Protocol client -*- lexical-binding: t -*-

;;; Commentary:

;; Here the configuration for LSP-Mode/Eglot.

;;; Code:

(leaf lsp-mode
  :commands lsp
  :init
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (java-mode-hook . lsp)
  (c-mode-hook    . lsp)
  (c++-mode-hook  . lsp)
  (nix-mode-hook  . lsp)
  (lsp-mode-hook  . lsp-enable-which-key-integration))

(leaf lsp-treemacs
   :after lsp)

(leaf lsp-java
  :ensure t
  :require t
  :config (add-hook 'java-mode-hook 'lsp))

;; optionally if you want to use debugger
;; (leaf dap-mode)
;; (leaf dap-LANGUAGE) to load the dap adapter for your language

(provide 'init-lsp)
;;; init-lsp.el ends here
