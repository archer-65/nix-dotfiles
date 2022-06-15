;;; init-fonts.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; Only font configuration, nothing to say.

;;; Code:

(defvar archer/font-height
  (if (string-equal (system-name) "quietfrost")
      180
    140))

(defun archer/font-setup ()
  "Simple function to initialize font, usually called with simple hook."
  ;; Global fonts
  (set-face-attribute 'default nil
                      :font "VictorMono Nerd Font"
                      :height archer/font-height)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      :font "VictorMono Nerd Font"
                      :height archer/font-height)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font "VictorMono Nerd Font"
                      :height archer/font-height
                      :weight 'light)
  (set-face-attribute 'org-modern-symbol nil
		      :family "Iosevka"))

;; Run this hook after we have initialized the first time.
(add-hook 'after-init-hook 'archer/font-setup)
;; Re-run this hook if we create a new frame from daemonized Emacs.
(add-hook 'server-after-make-frame-hook 'archer/font-setup)

(provide 'init-fonts)
;;; init-fonts.el ends here
