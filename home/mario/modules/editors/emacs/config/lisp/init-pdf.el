;;; init-pdf.el --- PDF reading customization, using pdf-tools -*- lexical-binding: t -*-

;;; Commentary:

;; Just pdf-tools installation and set as default

;;; Code:

(setup pdf-tools
  (unless (archer-using-nix-p)
    (elpaca pdf-tools
      (pdf-tools-install :no-query)))

  (:option display-buffer-alist '(("^\\*outline"
                                   display-buffer-in-side-window
                                   (side . left)
                                   (window-width . 0.20)
                                   (inhibit-switch-frame . t))))

  (:with-mode pdf-view-mode
    (:file-match "\\.[pP][dD][fF]\\'")))

(setup-pkg saveplace-pdf-view
  (:load-after pdf-tools))

(provide 'init-pdf)
;;; init-pdf.el ends here
