;;; init-code-style.el --- Code style settings -*- lexical-binding: t -*-

;;; Commentary:

;; OCD, so I have to remove useless whitespace after save or on demand.
;; Plus, general tab settings.

;;; Code:

(leaf format-all
  :doc "Same command to auto-format source code in many languages"
  :straight t
  :blackout t
  :hook
  (prog-mode-hook . format-all-mode)
  ;; (prog-mode-hook . format-all-ensure-formatter)
  :bind
  ("<f1>" . format-all-buffer))

(leaf eldoc
  :blackout t
  :config
  (global-eldoc-mode 1))

(leaf rainbow-mode
  :doc "Minor mode to set background of string matching hex colors to the hex color."
  :straight t
  :hook
  ((emacs-lisp-mode web-mode json-mode) . rainbow-mode))

(leaf ethan-wspace
  :doc "Delete useless whitespaces"
  :straight t
  :blackout t
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
  :straight t
  :require t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf tree-sitter
  :straight t
  :commands (tree-sitter-mode tree-sitter-hl-mode)
  :hook
  ((emacs-lisp-mode c-mode c++-mode java-mode python-mode) . tree-sitter-mode)
  (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(leaf tree-sitter-langs
  :straight t
  :after treesitter)

(provide 'init-code-style)
;;; init-code-style.el ends here
