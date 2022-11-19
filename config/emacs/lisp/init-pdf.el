;;; init-pdf.el --- PDF reading customization, using pdf-tools -*- lexical-binding: t -*-

;;; Commentary:

;; Just pdf-tools installation and set as default

;;; Code:

;; ???
;; (pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
;; (pdf-view-mode-hook . pdf-tools-enable-minor-modes)

(setup (:pkg pdf-tools)
  (:option display-buffer-alist '(("^\\*outline"
                                   display-buffer-in-side-window
                                   (side . left)
                                   (window-width . 0.20)
                                   (inhibit-switch-frame . t))))

  (:with-mode pdf-view-mode
    (:file-match "\\.[pP][dD][fF]\\'"))

  (pdf-tools-install :no-query))

(setup (:pkg saveplace-pdf-view)
  (:load-after pdf-tools))

(provide 'init-pdf)
;;; init-pdf.el ends here
