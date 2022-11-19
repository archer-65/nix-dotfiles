;;; init-shell.el --- Emacs <3 Shell -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain `eshell', `vterm', and similar terminal emulators available for Emacs.

;;; Code:

(setup vterm
  (unless (archer-using-nix-p) (straight-use-package 'vterm))
  (:autoload vterm vterm-other-window)

  (add-to-list 'display-buffer-alist '("^\\*vterm"
                                       (display-buffer-in-side-window)
                                       (window-height . 0.25)
                                       (side . bottom)
                                       (slot . 0)))

  (:option vterm-buffer-name-string "vterm: %s"
           vterm-max-scrollback 5000
           vterm-kill-buffer-on-exit t))

(setup (:straight multi-vterm)
  (:load-after vterm)
  (:option multi-vterm-dedicated-window-height-percent 20))

(provide 'init-shell)
;;; init-shell.el ends here
