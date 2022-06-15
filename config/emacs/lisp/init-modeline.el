;;; init-modeline.el --- Modeline customization -*- lexical-binding: t -*-

;;; Commentary:

;; Modeline customization and other useless/cute packages.

;;; Code:

(unless (version< emacs-version "28")
  (setq mode-line-compact nil))

(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		" "
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
		'(:eval (list (nyan-create)))
                "  "
                mode-line-position
                "  "
                (vc-mode vc-mode)
                "  "
                mode-line-misc-info
                mode-line-modes
                mode-line-end-spaces))

(setq mode-line-position-column-line-format '("<%l:%c>")) ; Emacs 28
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

;; Try this for upper mode-line
;; (setq-default header-line-format mode-line-format)
;; (setq-default mode-line-format nil)
;; (push '(internal-border-width . 8) default-frame-alist)
;; (setq-default left-margin-width 1)
;; (setq-default right-margin-width 1)
;; (setq-default fringes-outside-margins t)

;; Stupid thing
(leaf nyan-mode
  :doc "Stupid mode to scroll b"
  :ensure t
  :config
  (nyan-mode 1)
  (setq nyan-bar-length 24
        nyan-wavy-trail nil))

;; Some bugs, disabled right now.
(leaf moody
  :doc "Tabs and ribbons"
  :ensure t
  :disabled t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

;; Hiding minor mode can be useful, but right now I have disabled this behavior.
(leaf minions
  :doc "Hide minor modes"
  :ensure t
  :disabled t
  :config
  (minions-mode 1))

;; Currently disabled because a bit broken
(leaf doom-modeline
  :doc "A very attractive and rich (yet still minimal) mode line configuration for Emacs."
  :ensure t
  :disabled t
  :hook
  (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width . 1)
  (doom-modeline-minor-modes . t)
  (doom-modeline-buffer-file-name-style . 'buffer-name))

(provide 'init-modeline)
;;; init-modeline.el ends here
