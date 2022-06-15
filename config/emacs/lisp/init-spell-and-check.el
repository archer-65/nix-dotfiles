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
  :config
  (global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path . 'inherit)
  (flycheck-idle-change-delay . 1.0)
  (flycheck-display-errors-delay . 0.25)
  (flycheck-emacs-lisp-initialize-packages . t))

(provide 'init-spell-and-check)
;;; init-spell-and-check.el ends here
