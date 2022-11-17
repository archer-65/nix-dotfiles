;;; init-appearance.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain themes related settings and minor appearance stuff.

;;; Code:

(setup appearance
  ;; A simple frame title
  (setq frame-title-format '("%b – Emacs")
	icon-title-format frame-title-format)

  ;; Stuff
  (setq display-time-default-load-average nil)
  (setq highlight-nonselected-windows nil)
  (setq echo-keystrokes 0.1)

  ;; Other graphical stuff
  (setq visible-bell nil)
  (setq x-gtk-use-system-tooltips nil)
  (setq x-stretch-cursor nil)

  ;; Dialogs
  (setq use-dialog-box nil		; Mouse events dialog
	use-file-dialog nil)		; Disable dialog for files

  ;; Cursor
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default cursor-type 'bar)

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
  (setq window-divider-default-places t)
  (window-divider-mode 1))

(setup (:straight modus-themes)
  ;; Preferences
  (setq modus-themes-region '(accented no-extend bg-only)
        modus-themes-org-blocks 'gray-background
	modus-themes-mixed-fonts nil
	modus-themes-deuteranopia nil
	modus-themes-intense-mouseovers nil
	modus-themes-variable-pitch-ui nil
	modus-themes-tabs-accented t
	modus-themes-fringes nil
	modus-themes-markup '(intense)
	modus-themes-syntax '(yellow-comments)
	modus-themes-lang-checkers '(straight-underline faint)
	modus-themes-hl-line '(intense)
	modus-themes-paren-match '(intense)
	modus-themes-links '(no-underline)
	modus-themes-box-buttons '(variable-pitch faint 0.9)
	modus-themes-prompts '(intense bold)
	modus-themes-completions '((matches . (extrabold background))
                                   (selection . (bold accented))
                                   (popup . (accented intense)))
	modus-themes-mail-citations 'intense
	modus-themes-subtle-line-numbers t
        modus-themes-mode-line '(borderless accented))

  ;; Overrides
  (setq modus-themes-operandi-color-overrides
        `((fg-window-divider-inner . "#ffffff")
          (fg-window-divider-outer . "#ffffff")))
  (setq modus-themes-vivendi-color-overrides
        `((fg-window-divider-inner . "#000000")
          (fg-window-divider-outer . "#000000")))

  ;; Needed for packaged version
  (modus-themes-load-themes)

  ;; Load!
  (modus-themes-load-operandi))

;; I set circadian in the configuration of my themes
(setup (:straight circadian)
  (:load-after modus-themes)
  (:option circadian-themes '(("8:00" . modus-operandi)
			      ("20:00" . modus-vivendi)))
  (circadian-setup))

;; You must run `all-the-icons-install-fonts` the first time.
(setup (:straight all-the-icons)
  (:require all-the-icons)
  (:load-after dired
    (:straight all-the-icons-dired)
    (:with-mode dired-mode
      (:hook all-the-icons-dired-mode)))
  (:load-after marginalia
    (:straight all-the-icons-completion)
    (all-the-icons-completion-mode 1)
    (:with-mode marginalia-mode
      (:hook all-the-icons-completion-marginalia-setup))))

(provide 'init-appearance)
;;; init-appearance.el ends here
