;;; init-help.el --- Sometimes we need help from someone/something :) -*- lexical-binding: t -*-

;;; Commentary:

;; The minibuffer is our best friend, let's use it more with extensions.

;;; Code:

(setup which-key
  (:elpaca t)
  (:hide-mode)
  (:option which-key-idle-delay 0.2)
  (which-key-mode 1))

(setup helpful
  (:disable t)
  (:elpaca t)
  (:bind "C-h f"    helpful-callable
         "C-h v"    helpful-variable
         "C-h k"    helpful-key
         "C-h C"    helpful-command
         "C-c C-d"  helpful-at-point))

(provide 'init-help)
;;; init-help.el ends here
