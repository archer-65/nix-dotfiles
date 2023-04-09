;;; init-appearance.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain themes related settings and minor appearance stuff.

;;; Code:

(setup appearance
  ;; A simple frame title
  (setq frame-title-format '("%b – Emacs")
        icon-title-format frame-title-format)


  ;; Stuff
  (setq calendar-date-style 'european)
  (setq display-time-default-load-average nil)
  (setq highlight-nonselected-windows nil)
  (setq echo-keystrokes 0.1)

  ;; Other graphical stuff
  (setq visible-bell nil)
  (setq x-gtk-use-system-tooltips t)
  (setq x-stretch-cursor nil)

  ;; Dialogs
  (setq use-dialog-box nil      ; Mouse events dialog
        use-file-dialog nil)    ; Disable dialog for files

  ;; Cursor
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default cursor-type 'bar)
  (blink-cursor-mode 0)

  ;; Bidirectional settings
  (setq-default bidi-display-reordering 'left-to-right)
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Lines related
  (setq-default truncate-lines nil)
  (setq-default visual-line-mode t)

  (setq-default indicate-buffer-boundaries nil))

(setup windows
  (setq window-resize-pixelwise nil)

  ;; Splitting around
  (setq split-width-threshold 160
        split-height-threshold nil)

  ;; Dividers
  (setq window-divider-default-right-width 8)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 0))

(setup (:pkg modus-themes)
  ;; Preferences
  (:option modus-themes-org-blocks 'gray-background
           modus-themes-mixed-fonts nil
           modus-themes-variable-pitch-ui nil)

  ;; Overrides
  (:option modus-themes-common-palette-overrides
           ;; Modeline
           '((bg-mode-line-active bg-blue-subtle)
             (fg-mode-line-active fg-main)
             (border-mode-line-active blue-intense)
             ;; Region
             (bg-region bg-lavender)
             (fg-region unspecified)
             ;; Mouse Hovers
             (bg-hover bg-yellow-intense)
             ;; Fringe
             (fringe unspecified)
             ;; Inline code in prose (markup)
             (prose-block fg-dim)
             (prose-code green-cooler)
             (prose-done green)
             (prose-macro magenta-cooler)
             (prose-metadata fg-dim)
             (prose-metadata-value fg-alt)
             (prose-table fg-alt)
             (prose-tag magenta-faint)
             (prose-todo red)
             (prose-verbatim magenta-warmer)
             ;; Syntax
             (comment yellow-faint)
             (string green-warmer)
             ;; Checkers
             (underline-err red-faint)
             (underline-warning yellow-faint)
             (underline-note cyan-faint)
             ;; Links - No underlines
             (underline-link unspecified)
             (underline-link-visited unspecified)
             (underline-link-symbolic unspecified)
             ;; Box buttons
             (bg-button-active bg-main)
             (fg-button-active fg-main)
             (bg-button-inactive bg-inactive)
             (fg-button-inactive "gray50")
             ;; Prompts
             (fg-prompt cyan)
             (bg-prompt bg-cyan-nuanced)
             ;; Completion
             (fg-completion-match-0 fg-main)
             (fg-completion-match-1 fg-main)
             (fg-completion-match-2 fg-main)
             (fg-completion-match-3 fg-main)
             (bg-completion-match-0 bg-blue-subtle)
             (bg-completion-match-1 bg-yellow-subtle)
             (bg-completion-match-2 bg-cyan-subtle)
             (bg-completion-match-3 bg-red-subtle)
             ;; Mail citations
             (mail-cite-0 blue)
             (mail-cite-1 yellow)
             (mail-cite-2 green)
             (mail-cite-3 magenta)
             (mail-part magenta-cooler)
             (mail-recipient cyan)
             (mail-subject red-warmer)
             (mail-other cyan-cooler)
             ;; Line numbers
             (fg-line-number-inactive "gray50")
             (fg-line-number-active fg-main)
             (bg-line-number-inactive unspecified)
             (bg-line-number-active unspecified)))

  (modus-themes-select 'modus-operandi))

;; I set circadian in the configuration of my themes
(setup (:pkg circadian)
  (:load-after modus-themes)
  (:option circadian-themes '(("8:00" . modus-operandi)
                              ("20:00" . modus-vivendi)))
  (circadian-setup))

;; You must run `all-the-icons-install-fonts` the first time.
(setup (:pkg all-the-icons)
  (:require all-the-icons))

(setup (:pkg ef-themes))

(provide 'init-appearance)
;;; init-appearance.el ends here
