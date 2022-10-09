;;; init-appearance.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain themes related settings and minor appearance stuff.

;;; Code:

;; Preference, if I'm using mouse, I want dialog-box.
(setq use-dialog-box t     ; Mouse events dialog (yes or no predicate)
      use-file-dialog nil) ; Disable dialog for files

;; Favor vertical splits over horizontal ones.
(setq split-width-threshold 160
      split-height-threshold nil)

(setq-default cursor-type 'bar) ; Cursor type default

(leaf display-line-numbers
  :doc "Line numbers configuration."
  :setq
  (display-line-numbers-type . 'relative)
  (display-line-numbers-width-start . nil)
  (display-line-numbers-grow-only . t)
  :hook
  (prog-mode-hook . display-line-numbers-mode))

;; Themes section
;; For packaged versions which must use `require':
(leaf modus-themes
  :doc "Wonderful built-in themes by Protesilaos Stavrou"
  :straight t
  :init
  (setq modus-themes-region '(accented no-extend bg-only) ;; Region highlight
        modus-themes-org-blocks 'gray-background ;; Org source blocks background
	modus-themes-mixed-fonts nil ;; Mixed fonts support, for tables etc.
	modus-themes-deuteranopia nil
	modus-themes-intense-mouseovers nil
	modus-themes-variable-pitch-ui nil ;; Use better font for modeline and UI
	modus-themes-tabs-accented t
	modus-themes-fringes nil ;; Fringes { nil, 'subtle, 'intense}
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
	modus-themes-mail-citations 'intense ; {nil,'intense,'faint,'monochrome}
	modus-themes-subtle-line-numbers nil
        modus-themes-mode-line '(borderless accented))
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi))


;; Change based on time
(leaf circadian
  :straight t
  :config
  (setq circadian-themes '(("8:00" . modus-operandi)
                           ("20:00" . modus-vivendi)))
  (circadian-setup))

;;; Minor tweaks

;; You must run `all-the-icons-install-fonts` the first time.
(leaf all-the-icons
  :doc "Needed for modeline and dired"
  :straight t
  :require t)

(provide 'init-appearance)
;;; init-appearance.el ends here
