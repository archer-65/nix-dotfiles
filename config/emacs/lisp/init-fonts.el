;;; init-fonts.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; Only font configuration, nothing to say.

;;; Code:

(defun archer--org-font-setup ()
  "Simple function to initialize font, usually called with simple hook."
  ;; Global fonts
  (set-face-attribute 'default nil
                      :font "VictorMono Nerd Font"
                      :height 180)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      :font "VictorMono Nerd Font"
                      :height 180)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font "VictorMono Nerd Font"
                      :height 180
                      :weight 'light))

;; Run this hook after we have initialized the first time.
(add-hook 'after-init-hook 'archer--org-font-setup)
;; Re-run this hook if we create a new frame from daemonized Emacs.
(add-hook 'server-after-make-frame-hook 'archer--org-font-setup)

(provide 'init-fonts)
;;; init-fonts.el ends here
