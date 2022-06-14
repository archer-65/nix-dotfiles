;;; init-help.el --- Sometimes we need help from someone/something :) -*- lexical-binding: t -*-

;;; Commentary:

;; The minibuffer is our best friend, let's use it more with extensions.

;;; Code:

(leaf which-key
  :doc "Useful panel that appears while pressing any partial binding."
  :ensure t
  :blackout t
  :setq
  (which-key-idle-delay . 0.5)
  :config
  (which-key-mode))

(leaf helpful
  :doc "Helpful informations in buffers."
  :disabled t
  :ensure t
  :bind
  (("C-h f"   . helpful-callable)
   ("C-h v"   . helpful-variable)
   ("C-h k"   . helpful-key)
   ("C-h C"   . helpful-command)
   ("C-c C-d" . helpful-at-point)))

(provide 'init-help)
;;; init-help.el ends here
