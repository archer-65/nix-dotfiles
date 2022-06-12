;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Add load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; We don't want customizations in `init.el`, instead we use `custom.el`.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Disable damn sleep!
;; Yep, it's mandatory, that's the worst keybind ever.
(global-unset-key (kbd "C-z"))

;; Require package management file.
(require 'init-packages)

;; The `no-littering` package to keep folders where we edit files and the Emacs configuration folder clean.
(leaf no-littering
  :doc "Keeps folders clean"
  :setq
  ;; The package `no-littering` doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (auto-save-file-name-transforms . `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(require 'init-help)

(require 'init-appearance)

(require 'init-fonts)

(require 'init-dash)

(require 'init-complete)

(require 'init-consult)

(require 'init-embark)

(require 'init-complete-in-buffer)

(require 'init-editing)

;; (require 'init-meow)

(require 'init-windows)

(require 'init-buffers)

(require 'init-dired)

(require 'init-projects)

(require 'init-code-style)

(require 'init-spell-and-check)

(require 'init-extra-modes)

(require 'init-snippets)

(require 'init-lsp)

(defun archer-65/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(leaf org
  ;:pin org
  :ensure t
  :require t
  :hook (org-mode-hook . archer-65/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-pretty-entities 't))

(leaf org-modern
  :ensure t
  :require t
  :after org
  :hook (org-mode-hook . org-modern-mode))

(defun archer-65/org-mode-visual-fill ()
  (setq visual-fill-column-width 170
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(leaf visual-fill-column
  :hook (org-mode-hook . archer-65/org-mode-visual-fill))

;; Date settings for org-mode
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats
      '("<%d %b %Y>" . "<%d/%m/%y %a %H:%M>"))

;; LaTeX export settings
(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

;; Newpage after TOC
(setq org-latex-toc-command "\\tableofcontents \\clearpage")

;; Article 
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))

;; Using minted for tables
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-minted-options '(("breaklines" "true")
                                 ("breakanywhere" "true")))

(leaf ox-reveal
  :ensure ox-reveal)

(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t))))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("bash" . "src bash"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cc" . "src c")))

(eval-after-load 'org
  '(setf org-highlight-latex-and-related '(latex)))

;; Automatically tangle our Emacs.org config file when we save it.
(defun archer-65/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name archer--config-path))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'archer-65/org-babel-tangle-config)))

(leaf emojify
  :hook (after-init . global-emojify-mode))

(leaf mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :require t
  :config

  ;; Use mu4e for sending e-mail
  (setq mail-user-agent 'mu4e-user-agent)

  (setq sendmail-program "/usr/bin/msmtp"
    message-sendmail-f-is-evil t
    message-sendmail-extra-arguments '("--read-envelope-from")
    send-mail-function 'smtpmail-send-it
    message-send-mail-function 'message-send-mail-with-sendmail)
    ;mu4e-compose-signature "Sent from Emacs")

  (with-eval-after-load "emojify"
    (delete 'mu4e-headers-mode emojify-inhibit-major-modes))

  (require 'mu4e-contrib)

  (setq shr-color-visible-luminance-min 60)
  (setq shr-color-visible-distance-min 5)
  (setq shr-use-colors nil)
  (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))

  ;; Load org-mode integration
  (require 'org-mu4e)

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-maildir "~/mails")
  (setq mu4e-main-buffer-hide-personal-addresses t)

  (setq mu4e-contexts
        (list
         ;; Gmail Account
         (make-mu4e-context
          :name "Gmail"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-sent-folder  . "/Gmail/[Gmail]/Sent Mail")
                  (mu4e-drafts-folder  . "/Gmail/[Gmail]/Drafts")
                  (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")
                  (mu4e-maildir-shortcuts .
                                          (("/Gmail/Inbox"     . ?i)
                                           ("/Gmail/[Gmail]/Sent Mail" . ?s)
                                           ("/Gmail/[Gmail]/Trash"     . ?t)
                                           ("/Gmail/[Gmail]/Drafts"    . ?d)))))
         ;; Outlook
         (make-mu4e-context
          :name "Outlook"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Outlook" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-drafts-folder  . "/Outlook/Drafts")
                  (mu4e-sent-folder  . "/Outlook/Sent")
                  (mu4e-refile-folder  . "/Outlook/Archive")
                  (mu4e-trash-folder  . "/Outlook/Deleted")
                  (mu4e-maildir-shortcuts .
                                          (("/Outlook/Inbox"   . ?i)
                                           ("/Outlook/Sent"    . ?s)
                                           ("/Outlook/Deleted" . ?t)
                                           ("/Outlook/Drafts"  . ?d)))))

         ;;UniNa
         (make-mu4e-context
          :name "Unina"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Unina" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-drafts-folder  . "/Unina/Bozze")
                  (mu4e-sent-folder  . "/Unina/Posta inviata")
                  (mu4e-trash-folder  . "/Unina/Deleted Items")
                  (mu4e-maildir-shortcuts .
                                          (("/Unina/Inbox"         . ?i)
                                           ("/Unina/Posta inviata" . ?s)
                                           ("/Unina/Deleted Items" . ?t)
                                           ("/Unina/Bozze"         . ?d)))))))
  ;; Set Bookmarks for all
  (setq  mu4e-bookmarks '(( :name  "Unread messages"
                            :query "flag:unread AND NOT flag:trashed"
                            :key ?u)
                          ( :name "Today's messages"
                            :query "date:today..now"
                            :key ?t)))

  (setq mu4e-context-policy 'pick-first)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Don't ask to quit... why is this the default?
  (setq mu4e-confirm-quit nil)
  (mu4e t))

(leaf mu4e-alert
  :after mu4e
  :init
  (mu4e-alert-set-default-style 'libnotify)

  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

  (defun mu4e-alert--get-mu4e-frame ()
    "Try getting a frame containing a mu4e buffer."
    (car (delq nil (mapcar (lambda (buffer)
                             (when (and buffer
                                        (get-buffer-window buffer t))
                               (window-frame (get-buffer-window buffer t))))
                           (list mu4e-main-buffer-name)))))

  (defun mu4e-alert-filter-repeated-mails (mails)
    "Filters the MAILS that have been seen already."
    (cl-remove-if (lambda (mail)
                    (prog1 (and (not mu4e-alert-notify-repeated-mails)
                                (ht-get mu4e-alert-repeated-mails
                                        (plist-get mail :message-id)))
                      (ht-set! mu4e-alert-repeated-mails
                               (plist-get mail :message-id)
                               t)))
                  mails))

  (setq mu4e-alert-notify-repeated-mails nil))

(leaf org-msg
  :after mu4e
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-default-alternatives '((new		. (text html))
                                       (reply-to-html	. (text html))
                                       (reply-to-text	. (text)))
        org-msg-convert-citation t)
  (org-msg-mode))
