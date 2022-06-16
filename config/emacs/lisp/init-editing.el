;;; init-editing.el --- Basic editing configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file is pretty simple, it only contains editing related utilities and preferences.
;; It's still experimental and very poor, so I only consider it a starting point.

;;; Code:

;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;;
;;; Scrolling

;; Enable smooth scroll
(unless (version< emacs-version "29")
  (pixel-scroll-precision-mode 1))

;; General tweaks
(setq scroll-preserve-screen-position t
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically re-centered.
      scroll-conservatively 101
      hscroll-margin 2
      hscroll-step 1
      scroll-margin 0
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Horizontal scrolling tweaks

;; Horizontal scrolling mouse events should actually scroll left and right.
;; (setq mouse-wheel-tilt-scroll t)
;; (global-set-key (kbd "<mouse-6>") (lambda ()
;;                                     (interactive)
;; 				    (if truncate-lines (scroll-right 1))))
;; (global-set-key (kbd "<mouse-7>") (lambda ()
;;                                     (interactive)
;; 				    (if truncate-lines (scroll-left 1))))

;;; Prog-mode preference for truncating lines
;; (add-hook 'prog-mode-hook #'(lambda ()
;;                              (setq truncate-lines t
;;                                    word-wrap nil)))

;; Editing enhancements for `prog-mode`
(add-hook 'prog-mode #'visual-line-mode)
(add-hook 'prog-mode #'hl-line-mode)

;;; Pairs? I forget to balance every kind of pair, I need this.
(electric-pair-mode 1)
(show-paren-mode 1)

;;; Protesilaos Stavrou docet, system clipboard should have priority among kill-ring
(setq save-interprogram-paste-before-kill t)

;;; Tweak around!!!
(leaf undo-tree
  :doc "Simpler undo/redo tree, with textual tree mode."
  :straight t
  :init
  (global-undo-tree-mode))

(leaf delsel
  :doc "Should be default IMHO."
  :blackout t
  :hook
  (after-init-hook . delete-selection-mode))

(leaf drag-stuff
  :doc "Drag stuff around with alt+arrows"
  :straight t
  :blackout t
  :init
  (drag-stuff-global-mode 1)
  :config
  (drag-stuff-define-keys))

(leaf goto-last-change
  :doc "Oops, forgot position of last edit? Go back. (Thanks again, Prot)"
  :straight t
  :bind
  ("C-z" . goto-last-change))

(leaf autorevert
  :doc "Every time I have to confirm buffer reverts, do it yourself, Emacs"
  :blackout t
  :setq
  (auto-revert-verbose . t)
  :hook
  (after-init-hook . global-auto-revert-mode))

(leaf rainbow-mode
  :doc "Minor mode to set background of string matching hex colors to the hex color."
  :straight t
  :hook
  ((emacs-lisp-mode web-mode json-mode) . rainbow-mode))

(provide 'init-editing)
;;; init-editing.el ends here
