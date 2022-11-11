;;; init-spell-and-check.el --- Spell and syntax checking based on modes -*- lexical-binding: t -*-

;;; Commentary:

;; Flyspell as spell checker, while Flycheck as syntax checker for prog-mode.

;;; Code:

(leaf flyspell
  :hook
  (text-mode-hook . (lambda () flyspell-mode 1))
  (prog-mode-hook . flyspell-prog-mode))

(leaf flycheck
  :straight t
  :commands (flycheck-list-errors flycheck-buffer)
  :init
  ;; HACK: without this, the keywords added by leaf-keywords don't
  ;; take effect in the checker process, so they all trigger an
  ;; "unrecognized keyword" error.
  (setq flycheck-emacs-lisp-check-form
	(if (string-match-p "leaf-keywords"
                            flycheck-emacs-lisp-check-form)
            ;; Don't do anything on subsequent evals
            flycheck-emacs-lisp-check-form
          (format
           "(progn %s %s)"
           '(progn
              (require 'leaf-keywords nil t)
              (leaf-keywords-init))
           flycheck-emacs-lisp-check-form)))
  :config
  (global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path . 'inherit)
  (flycheck-idle-change-delay . 1.0)
  (flycheck-display-errors-delay . 0.25)
  (flycheck-emacs-lisp-initialize-packages . t))

(provide 'init-spell-and-check)
;;; init-spell-and-check.el ends here
