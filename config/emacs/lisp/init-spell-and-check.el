;;; init-spell-and-check.el --- Spell and syntax checking based on modes -*- lexical-binding: t -*-

;;; Commentary:

;; Flyspell as spell checker, while Flycheck as syntax checker for prog-mode.

;;; Code:

(leaf flyspell
  :hook
  (text-mode-hook . (lambda () flyspell-mode 1)))

(leaf flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay . 1.5))

(provide 'init-spell-and-check)
;;; init-spell-and-check.el ends here
