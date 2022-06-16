;;; init-complete-in-buffer.el --- In buffer completion configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Corfu/Company and extra Dabbrev configuration if needed.

;;; Code:

(leaf company
    ;; :after lsp-mode
    :straight t
    :init
    (global-company-mode)
    :bind
    (company-active-map
          ("<tab>" . company-complete-selection)))

(provide 'init-complete-in-buffer)
;;; init-complete-in-buffer.el ends here
