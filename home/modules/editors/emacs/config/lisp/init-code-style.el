;;; init-code-style.el --- Code style settings -*- lexical-binding: t -*-

;;; Commentary:

;; OCD, so I have to remove useless whitespace after save or on demand, and format all my code.
;; Plus, general tab settings, tree-sitter support, fancy stuff.

;;; Code:

(setup (:pkg format-all)
  (:blackout)
  (:hook-into prog-mode)
  (:global "<f1>" format-all-buffer))

(setup (:pkg editorconfig)
  (:blackout)
  (editorconfig-mode 1))

(setup eldoc
  (:blackout)
  (global-eldoc-mode 1))

(setup (:pkg rainbow-mode)
  (:hook-into web-mode json-mode))

(setup (:pkg ethan-wspace)
  (:blackout)
  (:global "C-c c" ethan-wspace-clean-all)
  (:hook-into prog-mode)
  ;; Required
  (:option mode-require-final-newline nil
           require-final-newline nil))

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
  (:hook-into nix-mode c-mode c++-mode java-mode python-mode)
  (:hooks tree-sitter-after-on-hook tree-sitter-hl-mode))

(setup (:pkg tree-sitter-langs)
  (:load-after treesitter))

(provide 'init-code-style)
;;; init-code-style.el ends here
