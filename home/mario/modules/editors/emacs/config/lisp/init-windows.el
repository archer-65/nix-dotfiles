;;; init-windows.el --- Windows navigation configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Only movement between buffers/frames, nothing special.

;;; Code:

(setup windmove
  ;; Windmove with shift+arrows
  (windmove-default-keybindings)
  (add-hook 'org-shiftup-final-hook    #'windmove-up)
  (add-hook 'org-shiftdown-final-hook  #'windmove-down)
  (add-hook 'org-shiftleft-final-hook  #'windmove-left)
  (add-hook 'org-shiftright-final-hook #'windmove-right))

(setup window
  (setq window-resize-pixelwise nil)

  ;; Splitting around
  (setq split-width-threshold 160
        split-height-threshold nil)

  ;; Dividers
  (setq window-divider-default-right-width 8)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 0)

  (keymap-global-set "C-x <up>"   'enlarge-window)
  (keymap-global-set "C-x <down>" 'shrink-window)
  (keymap-global-set "C-x {"      'shrink-window-horizontally)
  (keymap-global-set "C-x }"      'enlarge-window-horizontally))

(setup beframe
  (:pkg t)
  (setopt beframe-functions-in-frames '(project-prompt-project-dir)
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

(setup ace-window
  (:pkg t)
  (keymap-global-set "M-o" 'ace-window)
  (keymap-global-set "M-O" 'ace-swap-window)
  (setopt aw-scope 'frame
          aw-dispatch-always t
          aw-minibuffer-flag t))

(setup avy
  (:pkg t)
  (keymap-global-set "M-g j" 'avy-goto-char-timer)
  (setopt avy-all-windows nil   ;; only current
           avy-all-windows-alt t ;; all windows with C-u
           avy-single-candidate-jump t
           avy-case-fold-search nil
           avy-timeout-seconds 0.5
           avy-style 'pre))

(provide 'init-windows)
;;; init-windows.el ends here
