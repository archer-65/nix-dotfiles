;;; init-org-languages.el --- Language related org settings -*- lexical-binding: t -*-

;;; Commentary:

;; We can execute code in org-mode, but also define structure templates
;; to insert blocks (like src blocks).
;; Tangling is also an important feature, let's use it.

;;; Code:

;; Org-Babel
(leaf org-babel-load-languages
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (shell . t))))

;; Tempo
(leaf org-tempo
  :after org
  :require t
  :config
  (add-to-list 'org-structure-template-alist '("bash" . "src bash"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cc" . "src c"))
  :hook
  (org-mode-hook . (lambda () (setq-local electric-pair-inhibit-predicate
                                          `(lambda (c)
                                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

;; Auto tangling
(defun archer-65/org-babel-tangle-config ()
  "Auto tangle configuration on save if we are in the right directory."
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name archer/config-path))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(leaf ob-tangle
  :hook
  (org-mode-hook . (lambda () (add-hook 'after-save-hook #'archer-65/org-babel-tangle-config))))

;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'archer-65/org-babel-tangle-config)))

(provide 'init-org-languages)
;;; init-org-languages.el ends here
