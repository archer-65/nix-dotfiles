;;; init-modeline.el --- Modeline customization -*- lexical-binding: t -*-

;;; Commentary:

;; Modeline customization and other useless/cute packages.

;;; Code:

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
  :doc "Stupid mode to scroll b"
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
  (moody-replace-front-line)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(leaf minions
  :doc "Hide minor modes"
  :ensure t
  :disabled t
  :init
  (minions-mode 1))

(provide 'init-modeline)
;;; init-modeline.el ends here
