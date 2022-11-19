;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(setup (:straight cmake-mode)
  (:file-match (rx (or "CmakeLists.txt" ".cmake") eos)))

(setup (:straight nix-mode)
  (:file-match (rx ".nix" eos)))

(setup (:straight markdown-mode)
  (:file-match (rx (or ".md" ".markdown" ".mdown") eos)))

(setup (:straight yaml-mode)
  (:file-match (rx (or ".yml" ".yaml") eos)))

(setup (:straight json-mode)
  (:file-match (rx ".json" eos)))

(setup (:straight rustic)
  (:file-match (rx ".rs" eos))
  (:option rustic-format-on-save nil ; There's `format-all-mode'
           rustic-lsp-client archer-lsp-client))

(setup (:straight terraform-mode)
  (:file-match (rx ".tf" eos)))

(setup (:straight company-terraform)
  (:autoload company-terraform)
  (:when-loaded
    (defun archer-cape-company-terraform()
      "Add completion at point functions made from company backends for `terraform'."
      (setq-local
       completion-at-point-functions
       (append (list (cape-company-to-capf #'company-terraform)) completion-at-point-functions)))
    (:hooks terraform-mode-hook archer-cape-company-terraform)))

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
