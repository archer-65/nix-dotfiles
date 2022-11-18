;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(setup (:straight cmake-mode)
  (add-to-list 'auto-mode-alist '("\\CMakeLists\\.txt\\'" . cmake-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode)))

(setup (:straight nix-mode)
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))

(setup (:straight markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(setup (:straight yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

(setup (:straight json-mode)
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(setup (:straight rustic)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
  (:option rustic-format-on-save nil ; There's `format-all-mode'
           rustic-lsp-client archer-lsp-client))

(setup (:straight terraform-mode)
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode)))

(setup (:straight company-terraform)
  (:autoload company-terraform)
  (:when-loaded
    (defun archer-cape-company-terraform()
      "Add completion at point functions made from company backends for `terraform'."
      (setq-local
       completion-at-point-functions
       (append (list (cape-company-to-capf #'company-terraform)) completion-at-point-functions)))
    (add-hook 'terraform-mode-hook 'archer-cape-company-terraform)))

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
