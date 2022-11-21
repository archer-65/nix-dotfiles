;;; init-windows.el --- Windows navigation configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Only movement between buffers/frames, nothing special.

;;; Code:

(setup windmove
  (windmove-default-keybindings) ; Windmove with shift+arrows
  (:hooks  org-shiftup-final-hook    windmove-up
           org-shiftdown-final-hook  windmove-down
           org-shiftleft-final-hook  windmove-left
           org-shiftright-final-hook windmove-right))

(setup window
  (:global "C-S-k" enlarge-window
           "C-S-j" shrink-window
           "C-S-h" shrink-window-horizontally
           "C-S-l" enlarge-window-horizontally))

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
