;;; init-org.el --- Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Org mode is certainly the killer feature of Emacs.
;; You can do anything, for example capturing of templates, export, markdown like editing.

;;; Code:

(defun archer-org-mode-setup ()
  "Set important modes for me while editing org documents.

- Setting variable-pitch allows different face definition;
- I prefer visual-line here, instead of truncating lines."
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(setup org
  ;; General
  (setopt org-adapt-indentation nil
           org-fold-catch-invisible-edits 'smart
           org-cycle-separator-lines 1
           org-auto-align-tags nil
           org-tags-column 0 ;; place tags directly next to headline text
           org-archive-mark-done nil
           org-startup-folded 'content
           org-insert-heading-respect-content t
           org-read-date-prefer-future 'time
           org-startup-folded 'showeverything
           org-startup-indented t

           ;; Prettify
           org-ellipsis " ⤵" ;; "…" "⤵"
           org-hide-leading-stars t
           org-pretty-entities t
           org-pretty-entities-include-sub-superscripts t
           org-hide-emphasis-markers t
           org-fontify-quote-and-verse-blocks t
           org-list-allow-alphabetical t
           org-highlight-latex-and-related '(native latex)
           org-image-actual-width 500

           ;; Date
           org-display-custom-times t
           org-time-stamp-custom-formats '("<%d %b %Y>" . "<%d/%m/%y %a %H:%M>")

           ;; Footnotes
           org-footnote-section nil   ;; place footnotes locally
           org-footnote-auto-adjust t ;; renumber footnotes

            ;; Insertion/Yanking
           org-M-RET-may-split-line '((default . t)) ;; don't split line when creating a new headline, list item, or table field
           org-yank-adjusted-subtrees t              ;; adjust subtrees to depth when yanked
           org-yank-folded-subtrees t                ;; fold subtrees on yank

           org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
           org-list-indent-offset 1 ;; increase sub-item indentation

           ;; Movement
           org-return-follows-link t ;; make RET follow links
           org-special-ctrl-a/e t    ;; better movement in headers

           ;; Searching
           org-imenu-depth 8   ;; scan to depth 8 w/imenu
           imenu-auto-rescan t ;; make sure imenu refreshes

           ;; Source block settings
           org-src-fontify-natively t         ;; use lang-specific fontification
           org-src-window-setup 'other-window ;; edit source in other window
           org-src-tab-acts-natively t        ;; use lang bindings
           org-confirm-babel-evaluate t       ;; confirm evaluation

           ;; Source blocks
           org-hide-block-startup nil
           org-src-preserve-indentation nil
           org-edit-src-content-indentation 2)

  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (shell . t)
                               (groovy . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  (:local-set completion-at-point-functions '(cape-dabbrev cape-file))

  (:with-after org-ctags
    (setopt org-ctags-open-link-functions '()))

  (setopt org-directory "~/projects/pkm"
           org-agenda-files '("inbox.org" "todo.org")

           ;; Standard refiling is awful
           org-refile-use-outline-path 'file
           org-refile-use-cache t
           org-refile-allow-creating-parent-nodes t
           org-outline-path-complete-in-steps nil

           org-refile-targets '((nil :maxlevel . 4)
                                (org-agenda-files :maxlevel . 4))

           org-log-done 'time
           org-log-into-drawer t
           org-log-state-notes-insert-after-drawers nil

           org-use-fast-todo-selection 'expert       ;; don't use popup window for todos
           org-enforce-todo-dependencies t           ;; don't set to DONE if children aren’t DONE
           org-enforce-todo-checkbox-dependencies t

           org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d!)")
                               (sequence "WAIT(w@/!)" "SOMEDAY(s)" "|" "KILL(k@/!)" ))

           org-capture-templates
           '(("t" "Task" entry
              (file+headline "inbox.org" "Tasks")
              "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
              :empty-lines 1)
             ("n" "Note" entry
              (file+headline "inbox.org" "Notes")
              "* %?\n\n %i"
              :empty-lines 1)))

  (:hook archer-org-mode-setup))

(setup org-appear
  (:pkg t)
  (:autoload org-appear-mode)
  (:hook-into org-mode)
  (setopt org-appear-autoemphasis t
           org-appear-autolinks nil
           org-appear-autosubmarkers t))

(setup org-modern
  (:pkg t)
  (:load-after org)
  (:hook-into org-mode)
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka Nerd Font")
  (setopt org-modern-label-border 1
           org-modern-hide-stars nil      ;; Compatibility with org-indent
           org-modern-block-fringe nil    ;; Bad
           org-modern-variable-pitch nil
           org-modern-timestamp t
           org-modern-table t
           org-modern-table-vertical 1
           org-modern-table-horizontal 0))

(setup org-modern-indent
  (:pkg (:host github :repo "jdtsmith/org-modern-indent"))
  (:hook-into org-indent-mode))

(setup olivetti
  (:pkg t)
  (:load-after org)
  (:hook-into org-mode)
  (setopt olivetti-body-width 0.75
           olivetti-minimum-body-width 75
           olivetti-style 'fancy))

(provide 'init-org)
;;; init-org.el ends here
