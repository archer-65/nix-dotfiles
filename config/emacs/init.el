;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Add load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; We don't want customizations in `init.el`, instead we use `custom.el`.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Disable damn sleep!
;; Yep, it's mandatory, that's the worst keybind ever.
(global-unset-key (kbd "C-z"))

;; Require package management file.
(require 'init-packages)

;; The `no-littering` package to keep folders where we edit files and the Emacs configuration folder clean.
(leaf no-littering
  :doc "Keeps folders clean"
  :ensure t
  :setq
  ;; The package `no-littering` doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (auto-save-file-name-transforms . `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(require 'init-help)

(require 'init-appearance)

(require 'init-fonts)

(require 'init-dash)

(require 'init-complete)

(require 'init-consult)

(require 'init-embark)

(require 'init-complete-in-buffer)

(require 'init-editing)

;; (require 'init-meow)

(require 'init-windows)

(require 'init-buffers)

(require 'init-dired)

(require 'init-org)

(require 'init-org-languages)

(require 'init-org-export)

(require 'init-projects)

(require 'init-code-style)

(require 'init-spell-and-check)

(require 'init-extra-modes)

(require 'init-snippets)

(require 'init-lsp)

(require 'init-mail)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
