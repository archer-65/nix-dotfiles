;;; init-snippets.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:

;; Remember code snippet for common functions? Bleah.

;;; Code:

(leaf yasnippet
  :straight t
  :hook
  (prog-mode-hook . yas-minor-mode)
  :config
  (yas-reload-all))

(leaf yasnippet-snippets
  :straight t
  :after yasnippet)

(provide 'init-snippets)
;;; init-snippets.el ends here
