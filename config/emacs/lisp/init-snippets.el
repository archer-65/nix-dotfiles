;;; init-snippets.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:

;; Remember code snippet for common functions? Bleah.

;;; Code:

(leaf yasnippet
  :straight t
  :blackout t
  :hook
  (prog-mode-hook . yas-minor-mode)
  :config
  (yas-reload-all))

(leaf yasnippet-snippets
  :straight t
  :after yasnippet)

(leaf cape-yasnippet
  :straight (cape-yasnippet :type git :host github :repo "elken/cape-yasnippet")
  :after yasnippet
  :config
  (add-to-list 'completion-at-point-functions #'cape-yasnippet)
  :hook
  (eglot-managed-mode-hook . (lambda () (add-to-list 'completion-at-point-functions #'cape-yasnippet)))
  :bind
  ("C-c p y" . cape-yasnippet))

(provide 'init-snippets)
;;; init-snippets.el ends here
