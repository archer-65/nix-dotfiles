;;; init-snippets.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:

;; Remember code snippet for common functions? Bleah.

;;; Code:

(setup (:pkg yasnippet)
  (:with-mode yas-minor-mode
    (:hide-mode)
    (:hook-into prog-mode))
  (:when-loaded
    (yas-reload-all)))

(setup (:pkg yasnippet-snippets)
  (:load-after yasnippet))

(setup (:pkg (cape-yasnippet :type git :host github :repo "elken/cape-yasnippet"))
    (:global "C-c p y" cape-yasnippet))

(provide 'init-snippets)
;;; init-snippets.el ends here
