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

(provide 'init-windows)
;;; init-windows.el ends here
