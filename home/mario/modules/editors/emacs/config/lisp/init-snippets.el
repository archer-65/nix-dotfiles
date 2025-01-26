;;; init-snippets.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:

;; Remember code snippet for common functions? Bleah.

;;; Code:

(elpaca-setup yasnippet
  (:with-mode yas-minor-mode
    (:hide-mode)
    (:hook-into prog-mode))
  (:when-loaded
    (yas-reload-all)))

(elpaca-setup yasnippet-snippets
  (:load-after yasnippet))

(elpaca-setup yasnippet-capf
  (:load-after cape)
  (:global "C-c p y" cape-yasnippet))

(provide 'init-snippets)
;;; init-snippets.el ends here
