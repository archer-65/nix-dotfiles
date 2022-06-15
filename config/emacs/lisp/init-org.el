;;; init-org.el --- Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Org mode is certainly the killer feature of Emacs.
;; You can do anything, for example capturing of templates, export, markdown like editing.

;;; Code:

(defun archer/org-mode-setup ()
  "Set important modes for me while editing org documents.

- Indentation to distinguish headings is essential;
- Setting variable-pitch allows different face definition;
- I prefer visual-line here, instead of truncating lines."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun archer/org-mode-visual-fill ()
  "Width of visual fill and centered text are configured here."
  (setq visual-fill-column-width 170
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(leaf org
  :straight t
  :require t
  :hook
  (org-mode-hook . archer/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-pretty-entities 't)
  ;; Enable live previews
  (setq org-highlight-latex-and-related '(latex))
  ;; Date settings
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%d %b %Y>" . "<%d/%m/%y %a %H:%M>"))
  ;; Languages for org-src
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(leaf org-modern
  :doc "Org bullets? Old."
  :straight t
  :after org
  :require t
  :hook (org-mode-hook . org-modern-mode))

(leaf visual-fill-column
  :doc "Center buffers, word processor like editing"
  :disabled t
  :straight t
  :hook
  (org-mode-hook . archer/org-mode-visual-fill))

(leaf olivetti
  :doc "Focused writing, like visual-fill-column, but seems better."
  :straight t
  :hook
  (org-mode-hook . olivetti-mode)
  :config
  (setq olivetti-body-width 0.75)
  (setq olivetti-minimum-body-width 75)
  (setq olivetti-style 'fancy))

(provide 'init-org)
;;; init-org.el ends here
