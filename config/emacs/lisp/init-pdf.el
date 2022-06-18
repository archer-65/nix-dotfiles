;;; init-pdf.el --- PDF reading customization, using pdf-tools -*- lexical-binding: t -*-

;;; Commentary:

;; Just pdf-tools installation and set as default

;;; Code:

(leaf pdf-tools
  :straight t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (display-buffer-alist . '(("^\\*outline"
			    display-buffer-in-side-window
			    (side . left)
			    (window-width . 0.20)
			    (inhibit-switch-frame . t))))
  :config
  (pdf-tools-install)
  (define-pdf-cache-function pagelabels)
  :hook
  (pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
  (pdf-view-mode-hook . pdf-tools-enable-minor-modes))

(leaf saveplace-pdf-view
  :straight t
  :require t
  :after pdf-tools)

(provide 'init-pdf)
;;; init-pdf.el ends here
