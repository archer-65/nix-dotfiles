;;; init-org-export.el --- Org exports configuration -*- lexical-binding: t -*-

;;; Commentary:

;; We can export in any format with org-mode, but we need some
;; tweaks to achieve good results.
;; Here are listed all the settings for ox-latex, ox-reveal, etc.

;;; Code:

;; LaTeX export
(leaf ox-latex
  :require t
  :after org
  :config
  ;; Newpage after TOC
  (setq org-latex-toc-command "\\tableofcontents \\clearpage")
  ;; LaTeX process
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
  ;; Previewing LaTeX fragments in org mode, default changed for bad quality.
  (setq org-latex-create-formula-image-program 'imagemagick)
  ;; Using minted for tables
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")))
  ;; LaTeX classes
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; Enable listings
  (setq org-latex-listings 't))

;; Reveal.js
(leaf ox-reveal
  :straight t
  :after org
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(provide 'init-org-export)
;;; init-org-export.el ends here
