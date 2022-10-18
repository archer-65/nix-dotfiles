;;; init-modeline.el --- Modeline customization -*- lexical-binding: t -*-

;;; Commentary:

;; Modeline customization and other useless/cute packages.

;;; Code:

(unless (version< emacs-version "28")
  (setq mode-line-compact nil))

(leaf doom-modeline
  :doc "A very attractive and rich (yet still minimal) mode line configuration for Emacs."
  :straight t
  ;:disabled t
  :hook
  (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width . 1)
  (doom-modeline-height . 30)
  (doom-modeline-minor-modes . nil)
  (doom-modeline-major-mode-icon . t)
  (doom-modeline-major-mode-color-icon . t)
  (doom-modeline-buffer-file-state-icon . t)
  (doom-modeline-buffer-file-name-style . 'truncate-upto-project))

;; Hiding minor mode can be useful, but right now I have disabled this behavior.
(leaf minions
  :doc "Hide minor modes"
  :straight t
  :disabled t
  :config
  (minions-mode 1))

(provide 'init-modeline)
;;; init-modeline.el ends here
