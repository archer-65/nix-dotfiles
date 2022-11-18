;;; init-snippets.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:

;; Remember code snippet for common functions? Bleah.

;;; Code:

(setup (:straight yasnippet)
  (:blackout)
  (:hooks prog-mode-hook yas-minor-mode)
  (:when-loaded
    (yas-reload-all)))

(setup (:straight yasnippet-snippets)
  (:load-after yasnippet))

(setup (:straight (cape-yasnippet :type git :host github :repo "elken/cape-yasnippet"))
  (:load-after yasnippet)

  (defun archer-add-cape-yasnippet ()
    (add-to-list 'completion-at-point-functions #'cape-yasnippet))

  (:when-loaded (archer-add-cape-yasnippet))

  (:hooks eglot-managed-mode-hook archer-add-cape-yasnippet)

  (:global "C-c p y" cape-yasnippet))

(provide 'init-snippets)
;;; init-snippets.el ends here
