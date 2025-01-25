;;; init-code-style.el --- Code style settings -*- lexical-binding: t -*-

;;; Commentary:

;; OCD, so I have to remove useless whitespace after save or on demand, and format all my code.
;; Plus, general tab settings, tree-sitter support, fancy stuff.

;;; Code:

(setup (:pkg format-all)
  (:hide-mode)
  (:hook-into prog-mode)
  (:global "<f1>" format-all-buffer))

(setup (:pkg editorconfig)
  (:hide-mode)
  (editorconfig-mode 1))

(setup eldoc
  (:hide-mode)
  (global-eldoc-mode 1))

(setup (:pkg rainbow-mode)
  (:hook-into web-mode json-mode))

(setup (:pkg ws-butler)
  (:require)
  (:hook-into prog-mode)
  (:option mode-require-final-newline t
           require-final-newline t))

;; Tabs, indentation, and the TAB key
(setup indent
  (:option tab-always-indent 'complete
           tab-first-completion 'word-or-paren-or-punct
           tab-width 2
           indent-tabs-mode nil)) ; Use spaces!

(setup (:pkg rainbow-delimiters)
  (:hook-into prog-mode))

(setup (:pkg tree-sitter)
  (:autoload tree-sitter-mode tree-sitter-hl-mode)

  (:with-hook tree-sitter-after-on-hook
    (:hook tree-sitter-hl-mode))

  (global-tree-sitter-mode 1))

(setup (:pkg tree-sitter-langs)
  (:load-after treesitter))

(provide 'init-code-style)
;;; init-code-style.el ends here
