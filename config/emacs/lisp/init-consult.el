;;; init-consult.el --- Consult completing read -*- lexical-binding: t -*-

;;; Commentary:

;; .

;;; Code:

(setup (:straight consult)
  (:require consult)
  (:doc "Practical commands based on the Emacs completion function completing-read.")

  ;; C-c bindings (mode specific)
  (:global "C-c h" consult-history
     "C-c M" consult-mode-command
     "C-c b" consult-bookmark
     "C-c k" consult-kmacro)

  ;; C-x bindings
  (:global "C-x M-c" consult-complex-command      ; orig. repeat-complex-command
     "C-x b"   consult-buffer               ; orig. switch-to-buffer
     "C-x 4 b" consult-buffer-other-window  ; orig. switch-to-buffer-other-window
     "C-x 5 b" consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame

  ;; [C]-[M]-# bindings for registers
  (:global "C-M-#" consult-register
           "M-#"   consult-register-load
           "C-#"   consult-register-store) ; orig. abbrev-prefix-mark (unrelated)

  ;; Other custom bindings
  (:global "M-y"   consult-yank-pop  ; orig. yank-po
           "C-h a" consult-apropos)  ; orig. apropos-comman

  ;; M-g bindings (goto-map)
  (:bind-into goto-map
    "g"   consult-goto-line     ; orig. goto-line
    "o"   consult-org-heading   ; Alternative: consult-org-heading
    "m"   consult-mark
    "k"   consult-global-mark
    "i"   consult-imenu
    "I"   consult-imenu-multi
    "e"   consult-compile-error
    "f"   consult-flymake)     ; Alternative: consult-flymake

  ;; M-s bindings (search-map)
  (:bind-into search-map
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
    "e" consult-isearch-history)  ; Isearch integration

  ;; ??? From wiki
  ;; (:bind-into isearch-mode-map
  ;;   "M-e" consult-isearch-history       ; orig. isearch-edit-string
  ;;   "M-s e" consult-isearch-history     ; orig. isearch-edit-string
  ;;   "M-s l" consult-line                ; needed by consult-line to detect isearch
  ;;   "M-s L" consult-line-multi)         ; needed by consult-line to detect isearch

  ;; Register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (:option register-preview-delay 0
     register-preview-function #'consult-register-format)

  ;; Optionally configure the narrowing key.
  (:option consult-narrow-key "<")

  ;; Use Consult to select xref locations with preview
  (:option xref-show-xrefs-function #'consult-xref
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
   consult--source-bookmark
   :preview-key (kbd "M-."))

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
  (:hooks completion-list-mode consult-preview-at-point-mode))

(setup (:straight consult-dir)
  (:load-after consult
    (:global "C-x C-d" consult-dir)
    (:bind-into minibuffer-local-completion-map
      "C-x C-d" consult-dir-maybe
      "C-x C-j" consult-dir-jump-file)))

(setup (:straight consult-eglot)
  (:load-after (consult eglot)
    (:global "M-g s" consult-eglot-symbols)))

(provide 'init-consult)
;;; init-consult.el ends here
