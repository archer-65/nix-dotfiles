;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(setup cmake-mode
  (:pkg t)
  (:match-file (rx (or "CmakeLists.txt" ".cmake") eos)))

(setup nix-mode
  (:pkg t)
  (:match-file (rx ".nix" eos)))

(setup markdown-mode
  (:pkg t)
  (:match-file (rx (or ".md" ".markdown" ".mdown") eos)))

(setup yaml-mode
  (:pkg t)
  (:match-file (rx (or ".yml" ".yaml") eos)))

(setup json-mode
  (:pkg t)
  (:match-file (rx ".json" eos)))

(setup rustic
  (:pkg t)
  (:match-file (rx ".rs" eos))
  (setopt rustic-format-on-save nil ; There's `format-all-mode'
           rustic-lsp-client archer-lsp-client))

(setup terraform-mode
  (:pkg t)
  (:match-file (rx ".tf" eos)))

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
