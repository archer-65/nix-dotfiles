;;; init-code-style.el --- Code style settings -*- lexical-binding: t -*-

;;; Commentary:

;; OCD, so I have to remove useless whitespace after save or on demand, and format all my code.
;; Plus, general tab settings, tree-sitter support, fancy stuff.

;;; Code:

(elpaca-setup format-all
  (:hide-mode)
  (:hook-into prog-mode)
  (:global "<f1>" format-all-buffer))

(elpaca-setup editorconfig
  (:hide-mode)
  (editorconfig-mode 1))

(setup eldoc
  (:hide-mode)
  (global-eldoc-mode 1))

(elpaca-setup rainbow-mode
  (:hook-into web-mode json-mode))

(elpaca-setup ws-butler
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

(elpaca-setup rainbow-delimiters
  (:hook-into prog-mode))

(setup treesit
  (:only-if (treesit-available-p))
  (:option treesit-font-lock-level 4))

(elpaca-setup treesit-auto
  (:only-if (treesit-available-p))

  (:load-after treesit)
  (:autoload global-treesit-auto-mode)

  (:option treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(elpaca-setup tree-sitter-langs
  (:only-if (or (version< emacs-version "29") (not (treesit-available-p))))

  (:load-after tree-sitter-langs))

(elpaca-setup tree-sitter
  (:only-if (or (version< emacs-version "29") (not (treesit-available-p))))

  (:load-after tree-sitter-langs)

  (:autoload tree-sitter-mode tree-sitter-hl-mode)

  (:with-hook tree-sitter-after-on-hook
    (:hook tree-sitter-hl-mode))

  (global-tree-sitter-mode 1))


(provide 'init-code-style)
;;; init-code-style.el ends here
