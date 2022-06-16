;;; init-windows.el --- Windows navigation configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Only movement between buffers/frames, nothing special.

;;; Code:

(leaf windmove
  :doc "Utility to move faster between buffers"
  :config
  (windmove-default-keybindings) ; Windmove with shift+arrows
  :hook
  (org-shiftup-final-hook    . windmove-up)
  (org-shiftdown-final-hook  . windmove-down)
  (org-shiftleft-final-hook  . windmove-left)
  (org-shiftright-final-hook . windmove-right))

(leaf window
  :bind
  ("C-S-k" . enlarge-window)
  ("C-S-j" . shrink-window)
  ("C-S-h" . shrink-window-horizontally)
  ("C-S-l" . enlarge-window-horizontally))

(leaf ace-window
  :bind
  ("M-o" . ace-window)
  ("M-O" . ace-swap-window)
  :init
  (setq aw-scope 'frame
        aw-dispatch-always t
	aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(leaf avy
  :init
  (setq avy-all-windows nil   ;; only current
	avy-all-windows-alt t ;; all windows with C-u
	avy-single-candidate-jump t
	avy-case-fold-search nil
	avy-timeout-seconds 0.5
	avy-style 'pre)
  :bind
  ("M-g j" . avy-goto-char-timer))

(provide 'init-windows)
;;; init-windows.el ends here
