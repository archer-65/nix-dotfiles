;;; init-complete.el --- Completion enhancement -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs' internal completion is awesome, why should you use Ivy/Helm and similar?
;; They're wonderful, but complex and for me are unnecessary.
;; I'm using Vertico, Orderless and Marginalia (monster trio) for rich, orderless completion style.

;;; Code:

(setup minibuffer
  ;; Answers
  (fset #'yes-or-no-p #'y-or-n-p)
  (setq read-answer-short t)
  (setq use-short-answers t)

  ;; Files
  (setq file-name-shadow-properties '(invisible t intangible t))
  (file-name-shadow-mode 1)

  ;; Behavior
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (:with-hook minibuffer-setup-hook
    (:hook cursor-intangible-mode)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(setup (:require savehist)
  (setq savehist-file (locate-user-emacs-file "var/savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (:with-hook after-init-hook
    (:hook savehist-mode)))

;; Vertico
(setup (:pkg (vertico :files (:defaults "extensions/*")))

  (:also-load vertico-indexed
              vertico-flat
              vertico-grid
              vertico-mouse
              vertico-quick
              vertico-buffer
              vertico-repeat
              vertico-reverse
              vertico-directory
              vertico-multiform
              vertico-unobtrusive)

  (:option vertico-scroll-margin 0
           vertico-count 12
           vertico-resize t
           vertico-cycle t)

  (:with-map vertico-map
    (:bind
      "<escape>" #'minibuffer-keyboard-quit))

  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "λ " 'face 'vertico-current)
                   "  ")
                 cand)))

  ;; Sort directories before files
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (:option vertico-multiform-commands
           '((dired (vertico-sort-function . sort-directories-first))))

  (:option vertico-multiform-categories
           '(
             ;; (consult-grep buffer)
             ;; (consult-ripgrep buffer)
             ;; (consult-git-grep buffer)
             ;; (consult-find buffer)
             (file (vertico-sort-function . sort-directories-first))))

  ;; This works with `file-name-shadow-mode'.  When you are in a
  ;; sub-directory and use, say, `find-file' to go to your home '~/' or
  ;; root '/' directory, Vertico will clear the old path to keep only
  ;; your current input.
  (:with-hook rfn-eshadow-update-overlay-hook
    (:hook vertico-directory-tidy))

  (:with-mode minibuffer-setup-hook
    (:hook vertico-repeat-save))


  (vertico-mode 1)
  (vertico-multiform-mode 1))

;; Marginalia
(setup (:pkg marginalia)
  (:load-after vertico)
  (:with-map minibuffer-local-map
    (:bind
      "M-A" marginalia-cycle))
  (marginalia-mode 1))

;; TODO: Remove fork when https://github.com/iyefrat/all-the-icons-completion/pull/33 is merged
;; TODO: Then, remove direct reference to GitHub when on MELPA
(setup (:pkg (all-the-icons-completion :type git :host github :repo "iyefrat/all-the-icons-completion"
                                       :fork (:host github
                                              :repo "maxecharel/all-the-icons-completion"
                                              :branch "contrib")))
  (:with-after (all-the-icons marginalia)
    (all-the-icons-completion-mode 1)
    (:with-mode marginalia-mode
      (:hook all-the-icons-completion-marginalia-setup))))

;; Orderless
(setup (:pkg orderless)
  (defun archer-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher, using equal sign as a suffix."
    (cond
     ((equal "=" pattern)
      '(orderless-literal . "="))
     ((string-suffix-p "=" pattern)
      (cons 'orderless-literal (substring pattern 0 -1)))))

  (defun archer-orderless-without-literal-dispatcher (pattern _index _total)
    "Literal without style dispatcher using the exclamation mark as a suffix."
    (cond
     ((equal "!" pattern)
      '(orderless-literal . "!"))
     ((string-suffix-p "!" pattern)
      (cons 'orderless-without-literal (substring pattern 0 -1)))))

  (defun archer-orderless-initialism-dispatcher (pattern _index _total)
    "Leading initialism dispatcher using comma as suffix."
    (cond
     ((equal "," pattern)
      '(orderless-literal . ","))
     ((string-suffix-p "," pattern)
      (cons 'orderless-initialism (substring pattern 0 -1)))))

  (defun archer-orderless-flex-dispatcher (pattern _index _total)
    "Flex dispatcher using the tilde suffix."
    (cond
     ((equal "~" pattern)
      '(orderless-literal . "~"))
     ((string-suffix-p "~" pattern)
      (cons 'orderless-flex (substring pattern 0 -1)))))

  (:option completion-styles '(orderless basic)
           orderless-component-separator 'orderless-escapable-split-on-space
           completion-category-defaults nil)

  (:option orderless-style-dispatchers
           '(archer-orderless-literal-dispatcher
             archer-orderless-without-literal-dispatcher
             archer-orderless-initialism-dispatcher
             archer-orderless-flex-dispatcher))

  (:option completion-category-overrides
           '((file (styles . (partial-completion basic orderless)))
             (project-file (styles . (partial-completion basic orderless))))))

(provide 'init-complete)
;;; init-complete.el ends here
