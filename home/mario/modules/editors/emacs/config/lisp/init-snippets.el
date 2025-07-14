;;; init-snippets.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:

;; Remember code snippet for common functions? Bleah.

;;; Code:

(setup yasnippet
  (:pkg t)
  (:with-mode yas-minor-mode
    (:hide-mode)
    (:hook-into prog-mode))
  (:when-loaded
    (yas-reload-all)))

(setup yasnippet-snippets
  (:pkg t)
  (:load-after yasnippet))

(setup yasnippet-capf
  (:pkg t)
  (:load-after cape)
  (:global "C-c p y" cape-yasnippet))

(provide 'init-snippets)
;;; init-snippets.el ends here
