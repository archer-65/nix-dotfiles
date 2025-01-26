;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(elpaca-setup cmake-mode
  (:file-match (rx (or "CmakeLists.txt" ".cmake") eos)))

(elpaca-setup nix-mode
  (:file-match (rx ".nix" eos)))

(elpaca-setup markdown-mode
  (:file-match (rx (or ".md" ".markdown" ".mdown") eos)))

(elpaca-setup yaml-mode
  (:file-match (rx (or ".yml" ".yaml") eos)))

(elpaca-setup json-mode
  (:file-match (rx ".json" eos)))

(elpaca-setup rustic
  (:file-match (rx ".rs" eos))
  (:option rustic-format-on-save nil ; There's `format-all-mode'
           rustic-lsp-client archer-lsp-client))

(elpaca-setup terraform-mode
  (:file-match (rx ".tf" eos)))

(elpaca-setup company-terraform
  (:autoload company-terraform)

  (:with-after (cape terraform-mode)
    (:with-mode terraform-mode
      ;; Add completion at point functions made from company backends for `terraform'
      (:local-set completion-at-point-functions (append
                                                 (list (cape-company-to-capf #'company-terraform))
                                                 completion-at-point-functions)))))

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
