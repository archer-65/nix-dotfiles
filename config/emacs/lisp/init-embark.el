;;; init-embark.el --- Embark, run a command based on point-*- lexical-binding: t -*-

;;; Commentary:

;; Sometimes you want to act near point, but there are many actions.
;; Embark ships many actions, dependant on target and modes.

;;; Code:

;; Embark configuration
(leaf embark
  :doc "Act near point :D"
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Used for export and edit after ripgrep magic.
(leaf wgrep
  :doc "Edit matches in place.")

;; Integration with Consult
(leaf embark-consult
  :ensure t
  :after (embark consult)
  :leaf-defer nil ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-embark)
;;; init-embark.el ends here
