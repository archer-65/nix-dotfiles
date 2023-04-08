;;; init-windows.el --- Windows navigation configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Only movement between buffers/frames, nothing special.

;;; Code:

(setup windmove
  ;; Windmove with shift+arrows
  (windmove-default-keybindings)
  (:hooks org-shiftup-final-hook    windmove-up
          org-shiftdown-final-hook  windmove-down
          org-shiftleft-final-hook  windmove-left
          org-shiftright-final-hook windmove-right))

(setup window
  (:global "C-x <up>"   enlarge-window
           "C-x <down>" shrink-window
           "C-x {"      shrink-window-horizontally
           "C-x }"      enlarge-window-horizontally))

(setup (:pkg beframe)
  (:option beframe-functions-in-frames '(project-prompt-project-dir)
           beframe-global-buffers '("*scratch*"
                                    "*Messages"
                                    "*Async-native-compile-log*"
                                    "*straight-byte-compilation*"
                                    "*straight-process*"
                                    "*dashboard*"))

  (:with-after consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defvar beframe--consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'beframe-buffer-names
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe--consult-source))

  (beframe-mode 1))

(setup (:pkg ace-window)
  (:global "M-o" ace-window
           "M-O" ace-swap-window)
  (setq aw-scope 'frame
        aw-dispatch-always t
        aw-minibuffer-flag t)
  (ace-window-display-mode 1))

(setup (:pkg avy)
  (:global "M-g j" avy-goto-char-timer)
  (setq avy-all-windows nil   ;; only current
        avy-all-windows-alt t ;; all windows with C-u
        avy-single-candidate-jump t
        avy-case-fold-search nil
        avy-timeout-seconds 0.5
        avy-style 'pre))

(provide 'init-windows)
;;; init-windows.el ends here
