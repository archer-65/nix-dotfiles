;;; init-org.el --- Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Org mode is certainly the killer feature of Emacs.
;; You can do anything, for example capturing of templates, export, markdown like editing.

;;; Code:

(defun archer-org-mode-setup ()
  "Set important modes for me while editing org documents.

- Indentation to distinguish headings is essential;
- Setting variable-pitch allows different face definition;
- I prefer visual-line here, instead of truncating lines."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(setup (:pkg org)
  ;; General
  (:option org-adapt-indentation nil
           org-auto-align-tags nil
           org-archive-mark-done nil
           org-cycle-separator-lines 1
           org-startup-folded 'content
           org-fold-catch-invisible-edit 'show-and-error
           ;; Prettify
           org-ellipsis "…"
           org-hide-emphasis-markers t
           org-pretty-entities t
           org-fontify-quote-and-verse-blocks t
           ;; Live previews
           org-highlight-latex-and-related '(native latex)
           ;; Date
           org-display-custom-times t
           org-time-stamp-custom-formats '("<%d %b %Y>" . "<%d/%m/%y %a %H:%M>"))

  ;; Source blocks
  (:option org-hide-block-startup nil
           org-src-preserve-indentation nil
           org-edit-src-content-indentation 2)

  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (shell . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  (:local-set completion-at-point-functions '(cape-dabbrev cape-file))

  (:hook archer-org-mode-setup))

(setup (:pkg org-modern)
  (:doc "Org bullets? Old.")
  (:load-after org)
  (:hook-into org-mode)
  (:face org-modern-symbol ((t (:family "Iosevka"))))
  (:option org-modern-label-border 1
           org-modern-block-fringe nil  ; Bad
           org-modern-variable-pitch nil
           org-modern-timestamp t
           org-modern-table t
           org-modern-table-vertical 1
           org-modern-table-horizontal 0))

(setup (:pkg olivetti)
  (:doc "Focused writing, like visual-fill-column, but seems better.")
  (:load-after org)
  (:hook-into org-mode)
  (:option olivetti-body-width 0.75
           olivetti-minimum-body-width 75
           olivetti-style 'fancy))

(provide 'init-org)
;;; init-org.el ends here
