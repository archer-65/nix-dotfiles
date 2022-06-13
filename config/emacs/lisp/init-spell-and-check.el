;;; init-spell-and-check.el --- Spell and syntax checking based on modes -*- lexical-binding: t -*-

;;; Commentary:

;; Flyspell as spell checker, while Flycheck as syntax checker for prog-mode.

;;; Code:

(leaf flyspell
  :hook
  (text-mode-hook . (lambda () flyspell-mode 1))
  (prog-mode-hook . flyspell-prog-mode))

(leaf flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path . 'inherit)
  (flycheck-display-errors-delay . 1.5)
  (flycheck-emacs-lisp-initialize-packages . t))
  ;:hook
  ;(emacs-lisp-mode-hook . lisp-interaction-mode-hook))

(provide 'init-spell-and-check)
;;; init-spell-and-check.el ends here
