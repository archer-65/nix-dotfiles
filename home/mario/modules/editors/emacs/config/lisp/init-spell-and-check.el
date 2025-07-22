;;; init-spell-and-check.el --- Spell and syntax checking based on modes -*- lexical-binding: t -*-

;;; Commentary:

;; Flyspell as spell checker, while Flycheck as syntax checker for prog-mode.

;;; Code:

(setup flymake
  ;; Dumb `flymake' made me crash for this
  (add-to-list 'elisp-flymake-byte-compile-load-path load-path)

  (:option flymake-fringe-indicator-position 'left-fringe
           flymake-suppress-zero-counters t
           flymake-start-on-flymake-mode t
           flymake-no-changes-timeout 0.3
           flymake-start-on-save-buffer t
           flymake-proc-compilation-prevents-syntax-check t
           flymake-wrap-around nil)

  (:option flymake-mode-line-format
           '("" flymake-mode-line-exception flymake-mode-line-counters))

  (:option flymake-mode-line-counter-format
           '(" " flymake-mode-line-error-counter
             flymake-mode-line-warning-counter
             flymake-mode-line-note-counter ""))

  (:with-map ctl-x-x-map
    (:bind "m" #'flymake-mode))

  (:bind "C-c ! s" #'flymake-start
         "C-c ! d" #'flymake-show-buffer-diagnostics ; Emacs28
         "C-c ! D" #'flymake-show-project-diagnostics ; Emacs28
         "C-c ! n" #'flymake-goto-next-error
         "C-c ! p" #'flymake-goto-prev-error)

  (:hook-into prog-mode text-mode))

;; From Purcell
(setup flymake-flycheck
  (:pkg t)
  (:with-after flycheck
    (setq-default flycheck-disabled-checkers
                  (append (default-value 'flycheck-disabled-checkers)
                          '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package)))

    (:with-mode flymake-mode
      (:local-set flymake-diagnostic-functions
                  (append flymake-diagnostic-functions
                          (flymake-flycheck-all-chained-diagnostic-functions))))))

(setup flyspell
  (:with-mode text-mode
    (:hook flyspell-mode))

  (:with-mode prog-mode
    (:hook flyspell-prog-mode)))

(provide 'init-spell-and-check)
;;; init-spell-and-check.el ends here
