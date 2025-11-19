;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(setup cmake-mode
  (:pkg t))

(setup nix-mode
  (:pkg t))

(setup markdown-mode
  (:pkg t))

(setup yaml-mode
  (:pkg t))

(setup json-mode
  (:pkg t))

(setup rustic
  (:pkg t)
  (setopt rustic-format-on-save nil ; There's `format-all-mode'
          rustic-lsp-client archer-lsp-client))

(setup terraform-mode
  (:pkg t))

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
