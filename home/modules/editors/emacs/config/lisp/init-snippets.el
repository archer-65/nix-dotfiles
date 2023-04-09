;;; init-snippets.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:

;; Remember code snippet for common functions? Bleah.

;;; Code:

(setup (:pkg yasnippet)
  (:blackout)
  (:hooks prog-mode-hook yas-minor-mode)
  (:when-loaded
    (yas-reload-all)))

(setup (:pkg yasnippet-snippets)
  (:load-after yasnippet))

(setup (:pkg (cape-yasnippet :type git :host github :repo "elken/cape-yasnippet"))
  (:load-after (cape yasnippet)
    (defun archer-add-cape-yasnippet ()
      (add-to-list 'completion-at-point-functions #'cape-yasnippet))

    (:with-mode eglot-managed-mode-hook
      (:hook archer-add-cape-yasnippet))

    (:global "C-c p y" cape-yasnippet)))


(provide 'init-snippets)
;;; init-snippets.el ends here
