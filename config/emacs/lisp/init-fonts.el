;;; init-fonts.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; Only font configuration, nothing to say.

;;; Code:

(defgroup archer-faces()
  "Extensions for faces."
  :group 'faces)

(defcustom archer-font-height 120
  "Variable that specifies the font height."
  :type 'integer
  :group 'archer-faces)

(defun archer-font-setup ()
  "Simple function to initialize font, usually called with a few hooks."
  ;; Global fonts
  (set-face-attribute 'default nil
                      :font "VictorMono Nerd Font"
                      :height archer-font-height)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      :font "VictorMono Nerd Font"
                      :height archer-font-height)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font "VictorMono Nerd Font"
                      :height archer-font-height
                      :weight 'light))

  ;; (set-face-attribute 'org-modern-symbol nil
  ;; 		      :family "Iosevka"))

;; (add-hook 'after-init-hook #'archer-font-setup)
;; (add-hook 'server-after-make-frame-hook #'archer-font-setup)

(setup faces
  (:option archer-font-height (if (string-equal (system-name) "quietfrost")
				  180
				140))
  ;; Run this hook after we have initialized the first time
  ;; and if we create a new frame from daemonized Emacs.
  (:with-hook (after-init-hook server-after-make-frame-hook)
    (:hook archer-font-setup)))

(provide 'init-fonts)
;;; init-fonts.el ends here
