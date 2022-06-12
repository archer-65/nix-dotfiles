;;; init-complete-in-buffer.el --- In buffer completion configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Corfu/Company and extra Dabbrev configuration if needed.

;;; Code:

(leaf company
    ;; :after lsp-mode
    :hook
    (after-init-hook . company-mode)
    (lsp-mode-hook . company-mode)
    :bind
    (company-active-map
          ("<tab>" . company-complete-selection))
    (lsp-mode-map
          ("<tab>" . company-indent-or-complete-common)))

(provide 'init-complete-in-buffer)
;;; init-complete-in-buffer.el ends here
