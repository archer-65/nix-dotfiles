;;; init-pdf.el --- PDF reading customization, using pdf-tools -*- lexical-binding: t -*-

;;; Commentary:

;; Just pdf-tools installation and set as default

;;; Code:

(setup pdf-tools
  (:pkg (:built-in 'prefer :post-install (pdf-tools-install :no-query)))
  (:require pdf-tools)

  (:option display-buffer-alist '(("^\\*outline"
                                   display-buffer-in-side-window
                                   (side . left)
                                   (window-width . 0.20)
                                   (inhibit-switch-frame . t))))

  (:with-mode pdf-view-mode
    (:file-match "\\.[pP][dD][fF]\\'")))


(setup saveplace-pdf-view
  (:pkg t)
  (:load-after pdf-tools))

(provide 'init-pdf)
;;; init-pdf.el ends here
