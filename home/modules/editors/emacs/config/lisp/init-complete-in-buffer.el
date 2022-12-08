;;; init-complete-in-buffer.el --- In buffer completion configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Corfu completion UI and Cape extensions for better completion at point.

;;; Code:

(setup (:pkg corfu)
  (global-corfu-mode)

  ;; Load and enable corfu-history
  (load "extensions/corfu-history")
  (load "extensions/corfu-popupinfo")

  (corfu-history-mode)
  (corfu-popupinfo-mode)

  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold t)

  ;; SECTION FOR SPECIAL FUNCTIONS
  ;; Movement
  (defun contrib-corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))

  (defun contrib-corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))

  (define-key corfu-map [remap move-beginning-of-line] #'corfu-beginning-of-prompt)
  (define-key corfu-map [remap move-end-of-line] #'corfu-end-of-prompt)

  ;; From Corfu's manual
  (defun contrib-corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'contrib-corfu-move-to-minibuffer)

  ;; Adapted from Corfu's manual.
  ;; (Found in Prot's configuration)
  (defun contrib-corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'contrib-corfu-enable-always-in-minibuffer 1)
  ;; END OF SECTION (TODO Refactor)

  (:option corfu-cycle t
     corfu-auto t
     corfu-separator ?\s
     corfu-quit-at-boundary nil
     corfu-quit-no-match t
     corfu-preview-current #'insert
     corfu-preselect-first t
     corfu-on-exact-match #'insert
     corfu-echo-documentation 0.25
     corfu-min-width 30
     corfu-scroll-margin 5)

  (:bind-into corfu-popupinfo-map
    "M-p" corfu-popupinfo-scroll-down
    "M-n" corfu-popupinfo-scroll-up
    "M-d" corfu-popupinfo-toggle))

(setup (:pkg kind-icon)
  (:load-after corfu
    (:option kind-icon-default-face 'corfu-default
       kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.7 :scale 1.0))
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(setup (:pkg cape)
  ;; Needed for company-backends!
  (setup (:pkg company)
    (:autoload company-grab))

  (dolist (backend '(cape-symbol cape-keyword cape-file cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend))

  (:global "C-c p p" completion-at-point
     "C-c p t" complete-tag
     "C-c p d" cape-dabbrev
     "C-c p h" cape-history
     "C-c p f" cape-file
     "C-c p k" cape-keyword
     "C-c p s" cape-symbol
     "C-c p a" cape-abbrev
     "C-c p i" cape-ispell
     "C-c p l" cape-line
     "C-c p w" cape-dict
     "C-c p \\" cape-tex
     "C-c p _" cape-tex
     "C-c p ^" cape-tex
     "C-c p &" cape-sgml
     "C-c p r" cape-rfc1345))

(provide 'init-complete-in-buffer)
;;; init-complete-in-buffer.el ends here