;;; init-tex.el --- TeX related configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Self-explanatory.  As I really like to write using LaTeX and Emacs,
;; features like AucTeX are a must have.

;;; Code:

(setup auctex
  (:pkg t)
  (:with-mode LaTeX-mode
    (:match-file "\\.tex\\'"))

  (setopt TeX-master nil
           TeX-output-dir "output"
           TeX-auto-save t
           TeX-parse-self t
           TeX-auto-untabify t
           TeX-clean-confirm nil
           TeX-electric-sub-and-superscript t
           TeX-electric-math '("$" . "$")
           TeX-quote-after-quote nil
           TeX-save-query nil
           TeX-source-correlate-method 'synctex
           TeX-source-correlate-start-server t
           TeX-view-program-selection '((output-pdf "xdg-open"))
           ;; TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))

           LaTeX-item-indent 0
           LaTeX-syntactic-comments t
           LaTeX-fill-break-at-separators nil

           tex-fontify-script nil
           font-latex-fontify-sectioning 1.0
  )

  (:with-hook TeX-after-compilation-finished-functions
    (:hook TeX-revert-document-buffer)))

(provide 'init-tex)
;;; init-tex.el ends here
