;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(setup-pkg cmake-mode
  (:file-match (rx (or "CmakeLists.txt" ".cmake") eos)))

(setup-pkg nix-mode
  (:file-match (rx ".nix" eos)))

(setup-pkg markdown-mode
  (:file-match (rx (or ".md" ".markdown" ".mdown") eos)))

(setup-pkg yaml-mode
  (:file-match (rx (or ".yml" ".yaml") eos)))

(setup-pkg json-mode
  (:file-match (rx ".json" eos)))

(setup-pkg rustic
  (:file-match (rx ".rs" eos))
  (:option rustic-format-on-save nil ; There's `format-all-mode'
           rustic-lsp-client archer-lsp-client))

(setup-pkg terraform-mode
  (:file-match (rx ".tf" eos)))

(setup-pkg company-terraform
  (:autoload company-terraform)

  (:with-after (cape terraform-mode)
    (:with-mode terraform-mode
      ;; Add completion at point functions made from company backends for `terraform'
      (:local-set completion-at-point-functions (append
                                                 (list (cape-company-to-capf #'company-terraform))
                                                 completion-at-point-functions)))))

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
