;;; init-shell.el --- Emacs <3 Shell -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain `eshell', `vterm', and similar terminal emulators available for Emacs.

;;; Code:

(setup vterm
  (if (archer-using-nix-p)
      (cl-pushnew 'vterm elpaca-ignored-dependencies)
    (elpaca vterm))

  (:autoload vterm vterm-other-window)

  (:option vterm-buffer-name-string "vterm: %s"
           vterm-max-scrollback 5000
           vterm-kill-buffer-on-exit t))

(setup-pkg multi-vterm
  (:with-after vterm
    (:option multi-vterm-dedicated-window-height-percent 20)))

(provide 'init-shell)
;;; init-shell.el ends here
