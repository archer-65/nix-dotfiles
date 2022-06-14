;;; init-editing.el --- Basic editing configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file is pretty simple, it only contains editing related utilities and preferences.
;; It's still experimental and very poor, so I only consider it a starting point.

;;; Code:

;;; Scrolling behavior
;; Enable smooth scroll
(unless (version< emacs-version "29")
  (pixel-scroll-precision-mode 1))

;; These four come from the C source code.
;; (And from Protesilaos)
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 10000) ; affects `scroll-step'
(setq-default scroll-margin 1)
(setq-default scroll-step 1)
(setq-default next-screen-context-lines 0)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-progressive-speed nil)

;; Horizontal scrolling mouse events should actually scroll left and right.
(setq mouse-wheel-tilt-scroll t)
(global-set-key (kbd "<mouse-6>") (lambda ()
                                    (interactive)
				    (if truncate-lines (scroll-right 1))))
(global-set-key (kbd "<mouse-7>") (lambda ()
                                    (interactive)
				    (if truncate-lines (scroll-left 1))))

;;; Prog-mode preference for truncating lines
(add-hook 'prog-mode-hook #'(lambda ()
                             (setq truncate-lines t
                                   word-wrap nil)))

;;; Pairs? I forget to balance every kind of pair, I need this.
(electric-pair-mode 1)
(show-paren-mode 1)

;;; Protesilaos Stavrou docet, system clipboard should have priority among kill-ring
(setq save-interprogram-paste-before-kill t)

;;; Tweak around!!!
(leaf undo-tree
  :doc "Simpler undo/redo tree, with textual tree mode."
  :ensure t
  :init
  (global-undo-tree-mode))

(leaf delsel
  :doc "Should be default IMHO."
  :hook
  (after-init-hook . delete-selection-mode))

(leaf drag-stuff
  :doc "Drag stuff around with alt+arrows"
  :ensure t
  :config
  (drag-stuff-mode t)
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(leaf goto-last-change
  :doc "Oops, forgot position of last edit? Go back. (Thanks again, Prot)"
  :ensure t
  :bind
  ("C-z" . goto-last-change))

(leaf autorevert
  :doc "Every time I have to confirm buffer reverts, do it yourself, Emacs"
  :setq
  (auto-revert-verbose . t)
  :hook
  (after-init-hook . global-auto-revert-mode))

(leaf rainbow-mode
  :doc "Minor mode to set background of string matching hex colors to the hex color."
  :ensure t
  :config
  (rainbow-mode t))

(provide 'init-editing)
;;; init-editing.el ends here
