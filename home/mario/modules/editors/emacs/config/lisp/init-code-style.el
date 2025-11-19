;;; init-code-style.el --- Code style settings -*- lexical-binding: t -*-

;;; Commentary:

;; OCD, so I have to remove useless whitespace after save or on demand, and format all my code.
;; Plus, general tab settings, tree-sitter support, fancy stuff.

;;; Code:

(setup format-all
  (:pkg t)
  (:hide-mode)
  (:hook-into prog-mode)
  (keymap-global-set "<f1>" 'format-all-buffer))

(setup editorconfig
  (:pkg t)
  (:hide-mode)
  (editorconfig-mode 1))

(setup eldoc
  (:hide-mode)
  (global-eldoc-mode 1))

(setup rainbow-mode
  (:pkg t)
  (:hook-into web-mode json-mode))

(setup ws-butler
  (:pkg t)
  (:require)
  (:hook-into prog-mode)
  (setopt mode-require-final-newline t
           require-final-newline t))

;; Tabs, indentation, and the TAB key
(setup indent
  (setopt tab-always-indent 'complete
           tab-first-completion 'word-or-paren-or-punct
           tab-width 2
           indent-tabs-mode nil)) ; Use spaces!

(setup rainbow-delimiters
  (:pkg t)
  (:hook-into prog-mode))

;; FIXME: What the hell is this...
(if (and (version< "29" emacs-version) (treesit-available-p))

    (progn
      (setup treesit
        (setopt treesit-font-lock-level 4))

      (setup treesit-auto
        (:pkg t)
        (:load-after treesit)
        (:autoload global-treesit-auto-mode)

        (setopt treesit-auto-install 'prompt)
        (global-treesit-auto-mode)))

  (progn

    (setup tree-sitter-langs
      (:pkg t)
      (:load-after tree-sitter-langs))

    (setup tree-sitter
      (:pkg t)
      (:load-after tree-sitter-langs)

      (:autoload tree-sitter-mode tree-sitter-hl-mode)

      (:with-hook tree-sitter-after-on-hook
        (:hook tree-sitter-hl-mode))

      (global-tree-sitter-mode 1))))

(provide 'init-code-style)
;;; init-code-style.el ends here
