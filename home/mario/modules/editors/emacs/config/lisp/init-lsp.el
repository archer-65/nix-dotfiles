;;; init-lsp.el --- Language Server Protocol client -*- lexical-binding: t -*-

;;; Commentary:

;; Here the configuration for LSP-Mode.

;;; Code:

;;
;;; NOTE: These are taken from https://github.com/doomemacs/doomemacs/blob/master/modules/tools/lsp/config.el
(defvar archer-lsp--default-read-process-output-max nil)
(defvar archer-lsp--default-gcmh-high-cons-threshold nil)
(defvar archer-lsp--optimization-init-p nil)

(define-minor-mode archer-lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  :group 'lsp
  (if (not archer-lsp-optimization-mode)
      (setq-default read-process-output-max archer-lsp--default-read-process-output-max
                    gcmh-high-cons-threshold archer-lsp--default-gcmh-high-cons-threshold
                    archer-lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless archer-lsp--optimization-init-p
      (setq archer-lsp--default-read-process-output-max (default-value 'read-process-output-max)
            archer-lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without the native JSON
      ;;        library, so we up the GC threshold to stave off GC-induced
      ;;        slowdowns/freezes. Doom uses `gcmh' to enforce its GC strategy,
      ;;        so we modify its variables rather than `gc-cons-threshold'
      ;;        directly.
      (setq-default gcmh-high-cons-threshold (* 2 archer-lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq archer-lsp--optimization-init-p t))))

(defcustom archer-lsp-client 'eglot
  "Preferred lsp-client."
  :type 'symbol
  :group 'lsp)

;;
;;; LSP-MODE

(setup-pkg lsp-mode
  :disabled
  (:autoload lsp)

  (:when-loaded
    ;; Function to enable corfu in lsp-mode
    (defun archer-lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless

    ;; LSP completion with Corfu
    (when (corfu-mode)
      (setq lsp-completion-provider :none)
      (add-hook 'lsp-completion-mode-hook #'archer-lsp-mode-setup-completion)))

  (:option lsp-keymap-prefix "C-c l"
           lsp-keep-workspace-alive nil
           lsp-auto-guess-root nil
           lsp-log-io nil
           lsp-restart 'auto-restart
           lsp-enable-symbol-highlighting t
           lsp-enable-on-type-formatting t
           lsp-signature-auto-activate nil
           lsp-signature-render-documentation t
           lsp-modeline-code-actions-enable nil
           lsp-modeline-diagnostics-enable nil
           lsp-headerline-breadcrumb-enable t
           lsp-semantic-tokens-enable nil
           lsp-eldoc-render-all t
           lsp-idle-delay 0.5
           lsp-enable-snippet t
           lsp-enable-folding nil
           lsp-enable-imenu t
           lsp-eldoc-hook '(lsp-hover))

  (:with-mode (c-mode c++-mode java-mode nix-mode rustic-mode cmake-mode terraform-mode)
    (:hook lsp-deferred))

  (:hook-into lsp-enable-which-key-integration))

(setup-pkg lsp-ui
  :disabled
  (:autoload lsp-ui-mode)
  (:hook-into lsp-mode)
  (:load-after lsp)
  (:when-loaded
    (:option lsp-ui-doc-enable t
             lsp-ui-doc-header t
             lsp-ui-doc-include-signature t
             lsp-ui-doc-border '(face-foreground 'default)
             lsp-ui-sideline-show-code-actions t
             lsp-ui-sideline-delay 0.05)))

(setup-pkg lsp-java
  (:quit)
  (:load-after lsp))

;;
;;; EGLOT

;; FIXME: This is not working as I expected. I tried multiple times
;; and `straight.el' is pulling dependencies instead of using built-in
;; packages.
(cl-pushnew 'eglot elpaca-ignored-dependencies)
(setup eglot
  ;; List of modes and servers
  (:when-loaded
    (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
    (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
    (add-to-list 'eglot-server-programs `(nix-mode . ("nixd"))))

  (:with-after (cape yasnippet)
    (:local-set completion-at-point-functions (list (cape-capf-super
                                                     #'eglot-completion-at-point
                                                     #'cape-yasnippet
                                                     #'cape-file))))

  ;; WIP: Test cache busting feature of cape
  (:with-after (cape)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

  ;; Hooks
  (:with-mode (c-mode c++-mode java-mode nix-mode rustic-mode terraform-mode)
    (:hook eglot-ensure)))

(setup-pkg eglot-java
  (:load-after eglot)
  (:with-mode (java-mode)
    (:hook eglot-java-mode)))

(setup (:if-feature gcmh)
  (:with-hook (eglot-managed-mode-hook lsp-mode-hook)
    (:hook archer-lsp-optimization-mode)))

(provide 'init-lsp)
;;; init-lsp.el ends here
