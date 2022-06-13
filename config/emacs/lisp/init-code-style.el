;;; init-code-style.el --- Code style settings -*- lexical-binding: t -*-

;;; Commentary:

;; OCD, so I have to remove useless whitespace after save or on demand.
;; Plus, general tab settings.

;;; Code:

(leaf ethan-wspace
  :doc "Delete useless whitespaces"
  :ensure t
  :bind ("C-c c" . ethan-wspace-clean-all)
  :hook (prog-mode-hook . ethan-wspace-mode)
  :config
  (leaf files
    :doc "Required by ethan-wspace"
    :config
    (setq mode-require-final-newline nil)
    (setq require-final-newline nil)))

;; Tabs, indentation, and the TAB key
(leaf indent
  :doc "Tab settings"
  (setq-default tab-always-indent 'complete)
  (setq-default tab-first-completion 'word-or-paren-or-punct)
  (setq-default tab-width 2)
  ;; Use spaces!
  (setq-default indent-tabs-mode nil))

(leaf rainbow-delimiters
  :ensure t
  :require t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf tree-sitter
  :ensure t
  ;; :commands (global-tree-sitter-mode tree-sitter-mode tree-sitter-hl-mode)
  :init
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(leaf tree-sitter-langs
  :ensure t)

(provide 'init-code-style)
;;; init-code-style.el ends here
