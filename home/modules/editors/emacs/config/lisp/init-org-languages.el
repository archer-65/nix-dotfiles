;;; init-org-languages.el --- Language related org settings -*- lexical-binding: t -*-

;;; Commentary:

;; We can execute code in org-mode, but also define structure templates
;; to insert blocks (like src blocks).
;; Tangling is also an important feature, let's use it.

;;; Code:

(setup org-tempo
  (:load-after org
    (add-to-list 'org-structure-template-alist '("bash" . "src bash"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("cc" . "src c"))
    (add-to-list 'org-structure-template-alist '("j" . "src java")))

  (:with-mode org-mode
    (:local-set electric-pair-inhibit-predicate
                `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(setup ob-tangle
  ;; Auto tangling
  (defun archer-org-babel-tangle-config ()
    "Auto tangle configuration on save if we are in the right directory."
    (when (string-equal (file-name-directory (buffer-file-name))
                        (expand-file-name archer-config-path))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

  (:with-mode org-mode
    (:with-hook after-save-hook
      (:hook archer-org-babel-tangle-config))))

(provide 'init-org-languages)
;;; init-org-languages.el ends here
