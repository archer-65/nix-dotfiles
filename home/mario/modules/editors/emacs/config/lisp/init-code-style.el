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
  (:hook-into web-mode js-json-mode))

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

;; TODO: Consider moving this to an autoloaded file instead
;; Ref: https://github.com/doomemacs/modules/blob/8966104447d0a3131a232187bde148f58b32a322/modules/tools/tree-sitter/autoload/compat-30.el
(when (and (treesit-available-p) (version< emacs-version "31.1"))
  ;; Backported from Emacs 31's treesit.el.
  (autoload 'treesit-ready-p "treesit")

  (defcustom treesit-auto-install-grammar 'never
    "Whether to install tree-sitter language grammar libraries when needed."
    :type '(choice (const :tag "Never install grammar libraries" never)
                   (const :tag "Always automatically install grammar libraries" always)
                   (const :tag "Ask whether to install missing grammar libraries" ask))
    :group 'treesit)

  (defun treesit-ensure-installed (lang)
    "Ensure that the grammar library for LANG is installed."
    (or (treesit-ready-p lang t)
        (when (or (eq treesit-auto-install-grammar 'always)
                  (and (eq treesit-auto-install-grammar 'ask)
                       (y-or-n-p (format "Tree-sitter grammar for `%s' is missing; install it?" lang))))
          (treesit-install-language-grammar lang)
          (treesit-ready-p lang))))

  (unless (boundp 'treesit-major-mode-remap-alist)
    (defvar treesit-major-mode-remap-alist nil))

  (defcustom treesit-enabled-modes nil
    "Specify what treesit modes to enable by default."
    :type '(choice
            (const :tag "Disable all automatic associations" nil)
            (const :tag "Enable all available ts-modes" t))
    :initialize #'custom-initialize-default
    :set (lambda (sym val)
           (set-default sym val)
           (when (treesit-available-p)
             (dolist (mapping treesit-major-mode-remap-alist)
               (setq major-mode-remap-alist
                     (if (or (eq val t) (memq (cdr mapping) val))
                         (cons mapping major-mode-remap-alist)
                       (delete mapping major-mode-remap-alist))))))))

;; TODO: While this is working, I would like to try moving the "consumers"
;; in the dedicated section. For example, if I install modes I could manage
;; the mapping in that section.
(when (treesit-available-p)
  (setup treesit
    (setopt treesit-font-lock-level 4)

    (setopt treesit-major-mode-remap-alist
            '((bash-mode . bash-ts-mode)
              (c-mode . c-ts-mode)
              (c++-mode . c++-ts-mode)
              (css-mode . css-ts-mode)
              (java-mode . java-ts-mode)
              (js-mode . js-ts-mode)
              (js-json-mode . json-ts-mode)
              (nix-mode . nix-ts-mode)
              (python-mode . python-ts-mode)
              (toml-mode . toml-ts-mode)
              (yaml-mode . yaml-ts-mode)))

    (setopt treesit-enabled-modes t)

    (autoload 'yaml-indent-line "yaml-mode" nil t)
    (add-hook 'yaml-ts-mode-hook
              (lambda ()
                (setq-local indent-line-function #'yaml-indent-line)))))

(provide 'init-code-style)
;;; init-code-style.el ends here
