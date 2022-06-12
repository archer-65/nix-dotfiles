;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(leaf nix-mode
  :mode "\\.nix\\'")

;; (leaf company-nixos-options)

(leaf yaml-mode)


(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
