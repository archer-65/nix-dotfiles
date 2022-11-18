;;; init-org-languages.el --- Language related org settings -*- lexical-binding: t -*-

;;; Commentary:

;; We can execute code in org-mode, but also define structure templates
;; to insert blocks (like src blocks).
;; Tangling is also an important feature, let's use it.

;;; Code:

;; Tempo
(defun archer-org-inhibit-minor-pair ()
  "Function to disable electric-pair on < character."
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
                 (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))

(setup org-tempo
  (:load-after org)
  (add-to-list 'org-structure-template-alist '("bash" . "src bash"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cc" . "src c"))
  (add-to-list 'org-structure-template-alist '("j" . "src java"))
  (:hooks org-mode-hook archer-org-inhibit-minor-pair))

(provide 'init-org-languages)
;;; init-org-languages.el ends here
