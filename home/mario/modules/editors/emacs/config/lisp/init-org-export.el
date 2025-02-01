;;; init-org-export.el --- Org exports configuration -*- lexical-binding: t -*-

;;; Commentary:

;; We can export in any format with org-mode, but we need some
;; tweaks to achieve good results.
;; Here are listed all the settings for ox-latex, ox-reveal, etc.

;;; Code:

;; LaTeX export
(setup ox-latex
  (:load-after ox)
  (:option org-latex-toc-command "\\tableofcontents \\clearpage"  ; Newpage after TOC
           ;; Enable listings
           org-latex-listings t
           ;; Previewing LaTeX fragments in org mode, default changed for bad quality.
           org-latex-create-formula-image-program 'imagemagick
           ;; Using minted for tables
           org-latex-listings 'minted
           org-latex-packages-alist '(("" "minted"))
           org-latex-minted-options '(("breaklines" "true")
                                      ("breakanywhere" "true"))
           ;; PDF process
           ;; '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f")
           org-latex-pdf-process '("pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
                                   "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
                                   "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; (add-to-list 'org-latex-listings-langs '(yaml "yaml"))
  ;; (add-to-list 'org-latex-listings-langs '(groovy "groovy"))

  ;; LaTeX base classes
  (:when-loaded (add-to-list 'org-latex-classes
                             '("org-plain-latex"
                               "\\documentclass{article}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                               ("\\section{%s}" . "\\section*{%s}")
                               ("\\subsection{%s}" . "\\subsection*{%s}")
                               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                               ("\\paragraph{%s}" . "\\paragraph*{%s}")
                               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

;; Reveal.js
(setup-pkg (ox-reveal :host github :repo "yjwen/org-reveal")
  (:load-after ox)
  (:option org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

;; Hugo
(setup-pkg ox-hugo
  (:load-after ox))

(provide 'init-org-export)
;;; init-org-export.el ends here
