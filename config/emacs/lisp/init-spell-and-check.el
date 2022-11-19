;;; init-spell-and-check.el --- Spell and syntax checking based on modes -*- lexical-binding: t -*-

;;; Commentary:

;; Flyspell as spell checker, while Flycheck as syntax checker for prog-mode.

;;; Code:

(setup flymake
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

  (:bind-into ctl-x-x-map
    "m" #'flymake-mode)

  (:bind "C-c ! s" #'flymake-start
         "C-c ! d" #'flymake-show-buffer-diagnostics ; Emacs28
         "C-c ! D" #'flymake-show-project-diagnostics ; Emacs28
         "C-c ! n" #'flymake-goto-next-error
         "C-c ! p" #'flymake-goto-prev-error)

  (:hook-into prog-mode text-mode))

;; From Purcell's dotfiles
(setup (:pkg flymake-flycheck)
  (:load-after flymake)
  (:when-loaded
    (defun sanityinc/enable-flymake-flycheck ()
      (setq-local flymake-diagnostic-functions
                  (append flymake-diagnostic-functions
                          (flymake-flycheck-all-chained-diagnostic-functions))))

  (:hooks flymake-mode sanityinc/enable-flymake-flycheck)))

;; (setup flycheck (:disable) (:pkg flycheck)
;;   (:autoload flycheck-list-errors flycheck-buffer)
;;   (:option flycheck-emacs-lisp-load-path 'inherit
;;            flycheck-idle-change-delay 1.0
;;            flycheck-display-errors-delay 0.25
;;            flycheck-emacs-lisp-initialize-packages t)
;;   (global-flycheck-mode))

(setup flyspell
  (:hooks text-mode-hook (lambda () flyspell-mode 1)
          prog-mode-hook flyspell-prog-mode))

(provide 'init-spell-and-check)
;;; init-spell-and-check.el ends here
