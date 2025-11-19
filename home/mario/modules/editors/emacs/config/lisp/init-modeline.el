;;; init-modeline.el --- Modeline customization -*- lexical-binding: t -*-

;;; Commentary:

;; Modeline customization and other useless/cute packages.

;;; Code:

(setup modeline
  (unless (version< emacs-version "28")
    (setq mode-line-compact nil)
    (setq mode-line-position-column-line-format '("[L%l:C%C]")))

  (setq mode-line-percent-position '(-3 "%P"))
  (setq mode-line-defining-kbd-macro
        (propertize " Macro" 'face 'mode-line-emphasis))

  (setq-default mode-line-format
                '("%e"
                  " "
                  mode-line-mule-info
                  " "
                  mode-line-modified
                  mode-line-remote
                  " "
                  mode-line-buffer-identification
                  "  "
                  mode-line-position
                  " "
                  (vc-mode vc-mode)
                  " "
                  mode-line-modes
                  mode-line-misc-info)))

;; <https://github.com/minad/recursion-indicator>.
(setup recursion-indicator
  (:pkg t)
  (setopt recursion-indicator-general (concat "general" (all-the-icons-material "cached" :v-adjust -0.1))
           recursion-indicator-minibuffer (concat "minibuffer " (all-the-icons-material "cached" :v-adjust -0.1)))

  (setq-default mode-line-modes
                (seq-filter (lambda (s)
                              (not (and (stringp s)
                                        (string-match-p
                                         "^\\(%\\[\\|%\\]\\)$" s))))
                            mode-line-modes))

  (recursion-indicator-mode 1))

;;; Keycast mode
(setup keycast
  (:pkg t)
  ;; For `keycast-mode'
  (setopt keycast-mode-line-window-predicate #'keycast-active-frame-bottom-right-p
           keycast-separator-width 1
           keycast-mode-line-remove-tail-elements nil
           keycast-mode-line-format "%3s%k%c%r"
           keycast-mode-line-insert-after 'mode-line-misc-info)

  ;; For `keycast-log-mode'
  (setopt keycast-log-format "%-20K%C\n"
           keycast-log-newest-first t
           keycast-log-frame-alist '((minibuffer . nil)))

  ;; Based on Prot's configuration
  (:when-loaded
    (dolist (input '(self-insert-command
                     org-self-insert-command))
      (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

    (dolist (event '(mouse-event-p
                     mouse-movement-p
                     mwheel-scroll))
      (add-to-list 'keycast-substitute-alist `(,event nil)))))

(provide 'init-modeline)
;;; init-modeline.el ends here
