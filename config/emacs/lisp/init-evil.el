;;; init-evil.el ---  Become evil. -*- lexical-binding: t -*-

;;; Commentary:

;; Vim emulation layer.

;;; Code:

(leaf evil
  :straight t
  :init
  ;; Pre-load configuration
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  ;; Activate the Evil
  (evil-mode 1))

(provide 'init-evil)
;;; init-evil.el ends here
