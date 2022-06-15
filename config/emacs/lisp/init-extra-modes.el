;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(leaf nix-mode
  :straight t
  :mode "\\.nix\\'")

(leaf yaml-mode
  :straight t
  :mode "\\.yml\\'")

(leaf json-mode
  :straight t
  :mode "\\.json\\'")

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
