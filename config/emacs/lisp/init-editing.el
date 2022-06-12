;;; init-editing.el --- Basic editing configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file is pretty simple, it only contains editing related utilities and preferences.
;; It's still experimental and very poor, so I only consider it a starting point.

;;; Code:

;;; Scrolling behaviour
;; Enable smooth scroll
(unless (version< "29" emacs-version)
  (add-hook 'after-init-hook 'pixel-scroll-precision-mode))

;; These four come from the C source code.
;; (And from Protesilaos, rofl)
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1) ; affects `scroll-step'
(setq-default scroll-margin 0)
(setq-default next-screen-context-lines 0)

(leaf undo-tree
  :doc "Simpler undo/redo"
  :ensure t
  :init
  (global-undo-tree-mode))

(leaf delsel
  :hook
  (after-init-hook . delete-selection-mode))

(leaf drag-stuff
  :doc "Drag stuff around with alt+arrows"
  :ensure t
  :config
  (drag-stuff-mode t)
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(provide 'init-editing)
;;; init-editing.el ends here
