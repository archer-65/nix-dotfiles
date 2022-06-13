;;; init-appearance.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain themes related settings and minor appearance stuff.

;;; Code:

;;; Basic settings
(defun archer--display-numbers-hook ()
  "Turn on line numbers mode for certain modes."
  (display-line-numbers-mode t))

(leaf display-line-numbers
  :doc "Line numbers configuration."
  :setq
  (display-line-numbers-type . t)
  :hook
  (prog-mode-hook . archer--display-numbers-hook))

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

;;; Minor tweaks

;; You must run `all-the-icons-install-fonts` the first time.
(leaf all-the-icons
  :doc "Needed for modeline and dired"
  :ensure t)

(leaf doom-modeline
  :doc "A very attractive and rich (yet still minimal) mode line configuration for Emacs."
  :ensure t
  :hook
  (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width . 1)
  (doom-modeline-minor-modes . t)
  (doom-modeline-buffer-file-name-style . 'buffer-name))

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
