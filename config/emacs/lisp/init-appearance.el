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

;;; Basic settings
(defun archer/display-numbers-hook ()
  "Turn on line numbers mode for certain modes."
  (display-line-numbers-mode t))

(leaf display-line-numbers
  :doc "Line numbers configuration."
  :setq
  (display-line-numbers-type . t)
  (display-line-numbers-width-start . nil)
  (display-line-numbers-grow-only . t)
  :hook
  (prog-mode-hook . archer/display-numbers-hook))

;; Themes section
;; For packaged versions which must use `require':
(leaf modus-themes
  :doc "Wonderful built-in themes by Protesilaos Stavrou"
  :straight t
  :init
  (setq modus-themes-region '(accented no-extend bg-only)
        modus-themes-org-blocks 'gray-background
        modus-themes-mode-line '(moody borderless accented))
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))


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

(leaf emojify
   :doc "Enhanced emoji support :D"
   :straight t
   :config
   ;; Don't inhibit for mu4e
   (delete 'mu4e-headers-mode emojify-inhibit-major-modes)
   :hook
   (after-init-hook . global-emojify-mode))

(provide 'init-appearance)
;;; init-appearance.el ends here
