;;; init-lsp.el --- Language Server Protocol client -*- lexical-binding: t -*-

;;; Commentary:

;; Here the configuration for LSP-Mode.

;;; Code:

(leaf lsp-mode
  :straight t
  :commands lsp
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
  ;; (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  ;; (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection '("nil"))
  ;;					:major-modes '(nix-mode)
  ;; 					:activation-fn (lsp-activate-on "nix")
  ;;					:server-id 'nix-nil))
  (setq lsp-keep-workspace-alive nil)
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :hook
  (c-mode-hook    . lsp)
  (c++-mode-hook  . lsp)
  (java-mode-hook . lsp)
  (nix-mode-hook  . lsp)
  (rustic-mode-hook . lsp)
  (cmake-mode . lsp-deferred)
  (terraform-mode . lsp-deferred)
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

;; Not working now, I need time to try :(
(leaf eglot
  :disabled t
  :init
  (unless (package-installed-p 'eglot) straight-use-package 'eglot)
  :config
  (setq rustic-lsp-client 'eglot)
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
  :hook
  (c-mode-hook    . eglot-ensure)
  (c++-mode-hook  . eglot-ensure)
  (java-mode-hook . eglot-ensure)
  (nix-mode-hook  . eglot-ensure)
  (rustic-mode-hook . eglot-ensure)
  (terraform-mode . eglot-ensure))

(leaf eglot-java
  :disabled t
  :straight t
  :require t
  :after eglot
  :config
  (eglot-java-init))

(provide 'init-lsp)
;;; init-lsp.el ends here
