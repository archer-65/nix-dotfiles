;;; init-appearance.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain themes related settings and minor appearance stuff.

;;; Code:

;;; Basic settings
(defun archer/display-numbers-hook ()
  "Turn on line numbers mode for certain modes."
  (display-line-numbers-mode t))

(leaf display-line-numbers
  :doc "Line numbers configuration."
  :setq
  (display-line-numbers-type . t)
  :hook
  (prog-mode-hook . archer/display-numbers-hook))

;;; Themes section

;; For packaged versions which must use `require':
(leaf modus-themes
  :doc "Wonderful built-in themes by Protesilaos Stavrou"
  :ensure t
  :init
  (setq modus-themes-region '(accented no-extend bg-only)
        modus-themes-org-blocks 'gray-background
        modus-themes-mode-line '(borderless accented))
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

;; Change based on time
(leaf circadian
  :ensure t
  :config
  (setq circadian-themes '(("8:00" . modus-operandi)
                           ("20:00" . modus-vivendi)))
  (circadian-setup))

;;; Minor tweaks

;; You must run `all-the-icons-install-fonts` the first time.
(leaf all-the-icons
  :doc "Needed for modeline and dired"
  :ensure t
  :require t)

(setq-default mode-line-format
		'("%e"
		  mode-line-front-space
                  (:propertize
		   (" "
                   mode-line-mule-info
                   mode-line-client
                   mode-line-modified
                   mode-line-remote)
		  display
		  (min-width
		   (5.0)))
		  '(:eval (list (nyan-create)))
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "  "
                  mode-line-position
                  "  "
                  "  "
                  mode-line-misc-info
                  (vc-mode vc-mode)
                  mode-line-modes
                  mode-line-end-spaces))

(leaf nyan-mode
  :ensure t
  :config
  (nyan-mode 1)
  (setq nyan-bar-length 20
        nyan-wavy-trail nil))

(leaf moody
  :doc "Tabs and ribbons"
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(leaf minions
  :doc "Hide minor modes"
  :ensure t
  :init
  (minions-mode 1))

(leaf emojify
  :doc "Enhanced emoji support :D"
  :ensure t
  :config
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  ;; Don't inhibit for mu4e
  (delete 'mu4e-headers-mode emojify-inhibit-major-modes)
  (bind-key* (kbd "C-x C-/") #'emojify-insert-emoji)
  :hook
  (after-init-hook . global-emojify-mode))

(provide 'init-appearance)
;;; init-appearance.el ends here
