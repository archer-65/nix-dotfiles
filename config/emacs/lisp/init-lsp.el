;;; init-lsp.el --- Language Server Protocol client -*- lexical-binding: t -*-

;;; Commentary:

;; Here the configuration for LSP-Mode.

;;; Code:

;;
;;; NOTE: These are taken from https://github.com/doomemacs/doomemacs/blob/master/modules/tools/lsp/config.el
(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimization-init-p nil)

(define-minor-mode +lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  :group 'lsp
  (if (not +lsp-optimization-mode)
      (setq-default read-process-output-max +lsp--default-read-process-output-max
                    gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                    +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq +lsp--default-read-process-output-max (default-value 'read-process-output-max)
            +lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without the native JSON
      ;;        library, so we up the GC threshold to stave off GC-induced
      ;;        slowdowns/freezes. Doom uses `gcmh' to enforce its GC strategy,
      ;;        so we modify its variables rather than `gc-cons-threshold'
      ;;        directly.
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))

;;
;;; LSP-MODE


(leaf lsp-mode
  :straight t
  :commands lsp
  :init
  ;; Function to enable corfu in lsp-mode
  (defun archer/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :config
  ;; LSP completion with Corfu
  (when (corfu-mode)
    (customize-set-variable lsp-completion-provider :none)
    (add-hook 'lsp-completion-mode-hook #'archer/lsp-mode-setup-completion))
  :custom
  (lsp-keymap-prefix . "C-c l")
  (lsp-keep-workspace-alive . nil)
  (lsp-auto-guess-root . nil)
  (lsp-log-io . nil)
  (lsp-restart . 'auto-restart)
  (lsp-enable-symbol-highlighting . t)
  (lsp-enable-on-type-formatting . t)
  (lsp-signature-auto-activate . nil)
  (lsp-signature-render-documentation . t)
  (lsp-modeline-code-actions-enable . nil)
  (lsp-modeline-diagnostics-enable . nil)
  (lsp-headerline-breadcrumb-enable . t)
  (lsp-semantic-tokens-enable . nil)
  (lsp-eldoc-render-all . t)
  (lsp-idle-delay . 0.5)
  (lsp-enable-snippet . t)
  (lsp-enable-folding . nil)
  (lsp-enable-imenu . t)
  (lsp-eldoc-hook . '(lsp-hover))
  :hook
  ((c-mode-hook c++-mode-hook java-mode-hook nix-mode-hook rustic-mode-hook cmake-mode-hook terraform-mode-hook) . lsp-deferred)
  (lsp-mode-hook . +lsp-optimization-mode)
  (lsp-mode-hook  . lsp-enable-which-key-integration))

(leaf lsp-ui
  :straight t
  :after lsp
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable . t)
  (lsp-ui-doc-header . t)
  (lsp-ui-doc-include-signature . t)
  (lsp-ui-doc-border . '(face-foreground 'default))
  (lsp-ui-sideline-show-code-actions . t)
  (lsp-ui-sideline-delay . 0.05))

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

;;
;;; EGLOT

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
  (eglot-managed-mode-hook . +lsp-optimization-mode)
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
