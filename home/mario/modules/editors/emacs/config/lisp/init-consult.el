;;; init-consult.el --- Consult completing read -*- lexical-binding: t -*-

;;; Commentary:

;; Consult provides commands based on Emacs `completion-read' functionality.  Here my basic configuration and key-bindings.  Totally WIP.

;;; Code:

(setup consult
  (:pkg t)
  (:require consult)

  ;; C-c bindings (mode specific)
  (keymap-global-set "C-c h" 'consult-history)
  (keymap-global-set "C-c M" 'consult-mode-command)
  (keymap-global-set "C-c b" 'consult-bookmark)
  (keymap-global-set "C-c k" 'consult-kmacro)

  ;; C-x bindings
  (keymap-global-set "C-x M-c" 'consult-complex-command)      ; orig. repeat-complex-command
  (keymap-global-set "C-x b"   'consult-buffer)               ; orig. switch-to-buffer
  (keymap-global-set "C-x 4 b" 'consult-buffer-other-window)  ; orig. switch-to-buffer-other-window
  (keymap-global-set "C-x 5 b" 'consult-buffer-other-frame)   ; orig. switch-to-buffer-other-frame

  ;; [C]-[M]-# bindings for registers
  (keymap-global-set "C-M-#" 'consult-register)
  (keymap-global-set "M-#"   'consult-register-load)
  (keymap-global-set "C-#"   'consult-register-store) ; orig. abbrev-prefix-mark (unrelated)

  ;; Other custom bindings
  (keymap-global-set "M-y"   'consult-yank-pop) ; orig. yank-po
  (keymap-global-set "C-h a" 'consult-apropos)  ; orig. apropos-comman

  ;; M-g bindings (goto-map)
  (:with-map goto-map
    (:bind
     "g"   consult-goto-line     ; orig. goto-line
     "o"   consult-org-heading   ; Alternative: consult-org-heading
     "m"   consult-mark
     "k"   consult-global-mark
     "i"   consult-imenu
     "I"   consult-imenu-multi
     "e"   consult-compile-error
     "f"   consult-flymake))     ; Alternative: consult-flymake

  ;; M-s bindings (search-map)
  (:with-map search-map
    (:bind
     "f" consult-find
     "F" consult-locate
     "g" consult-grep
     "G" consult-git-grep
     "r" consult-ripgrep
     "l" consult-line
     "L" consult-line-multi
     "m" consult-multi-occur
     "k" consult-keep-lines
     "u" consult-focus-lines
     "e" consult-isearch-history))  ; Isearch integration

  ;; Isearch map integration
  (:with-map isearch-mode-map
    (:bind
     "M-e" consult-isearch-history       ; orig. isearch-edit-string
     "M-s e" consult-isearch-history     ; orig. isearch-edit-string
     "M-s l" consult-line                ; needed by consult-line to detect isearch
     "M-s L" consult-line-multi))        ; needed by consult-line to detect isearch

  ;; Register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setopt register-preview-delay 0
           register-preview-function #'consult-register-format)

  ;; Optionally configure the narrowing key.
  (setopt consult-narrow-key "<")

  ;; Use Consult to select xref locations with preview
  (setopt xref-show-xrefs-function #'consult-xref
           xref-show-definitions-function #'consult-xref)

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark :preview-key "M-.")

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  (:with-mode completion-list-mode
    (:hook consult-preview-at-point-mode)))

(setup consult-dir
  (:pkg t)
  (:with-after consult
    (keymap-global-set "C-x C-d" 'consult-dir)
    (:with-map minibuffer-local-completion-map
      (:bind
       "C-x C-d" consult-dir-maybe
       "C-x C-j" consult-dir-jump-file))))

(setup consult-eglot
  (:pkg t)
  (:with-after (consult eglot)
    (keymap-global-set "M-g s" 'consult-eglot-symbols)))

(provide 'init-consult)
;;; init-consult.el ends here
