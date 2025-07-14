;;; init-fonts.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; Only font configuration, nothing to say.

;;; Code:

(defgroup archer-faces()
  "Extensions for faces."
  :group 'faces)

(defcustom archer-font-height 140
  "Variable that specifies the font height."
  :type 'integer
  :group 'archer-faces)

(setup fontaine
  (:pkg t)
  (:option x-underline-at-descent-line nil
           use-default-font-for-symbols t)

  (unless (version< emacs-version "28")
    (setq-default text-scale-remap-header-line t))

  (:option archer-font-height (pcase (system-name)
                                ("quietfrost" 140)
                                ("mate" 140)
                                (_ 140)))


  ;; TODO: fix this, the state is not re-stored correctly.
  ; (:option fontaine-latest-state-file (locate-user-emacs-file "var/fontaine-state.eld"))

  (:option fontaine-presets
           `((victor
              :default-family "VictorMono Nerd Font"
              :default-height ,archer-font-height

              :variable-pitch-family "VictorMono Nerd Font")))

  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'victor))

  ;; NOTE: Is this really obsolete?
  ;; (:with-hook (modus-themes-after-load-theme-hook ef-themes-post-load-hook)
  ;;   (:hook fontaine-apply-current-preset))

  (:with-hook kill-emacs-hook
    (:hook fontaine-store-latest-preset)))

(provide 'init-fonts)
;;; init-fonts.el ends here
