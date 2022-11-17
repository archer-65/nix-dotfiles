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
		  mode-line-front-space
		  mode-line-client
		  "  "
		  mode-line-mule-info
		  "  "
		  mode-line-modified
		  mode-line-remote
		  mode-line-frame-identification
		  mode-line-buffer-identification
		  "  "
		  mode-line-position
		  (vc-mode vc-mode)
		  "  "
		  mode-line-modes
		  mode-line-misc-info
		  mode-line-end-spaces)))


;; <https://github.com/minad/recursion-indicator>.
(setup (:straight recursion-indicator)
  (:option recursion-indicator-general (concat "general" (all-the-icons-material "cached" :v-adjust -0.1))
	   recursion-indicator-minibuffer (concat "minibuffer " (all-the-icons-material "cached" :v-adjust -0.1)))

  (setq-default mode-line-modes
		(seq-filter (lambda (s)
			      (not (and (stringp s)
					(string-match-p
					 "^\\(%\\[\\|%\\]\\)$" s))))
                            mode-line-modes))

  (recursion-indicator-mode 1))

;;; Keycast mode
(setup (:straight keycast)
  ;; Those are for `keycast-mode'
  (setq keycast-mode-line-window-predicate #'keycast-active-frame-bottom-right-p)
  (setq keycast-separator-width 1)
  (setq keycast-mode-line-remove-tail-elements nil)
  (setq keycast-mode-line-format "%3s%k%c%r")
  (setq keycast-mode-line-insert-after 'mode-line-misc-info)

  ;; Based on Prot's configuration
  ;; (dolist (input '(self-insert-command
  ;; 		   org-self-insert-command))
  ;;   (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  ;; (dolist (event '(mouse-event-p
  ;; 		   mouse-movement-p
  ;; 		   mwheel-scroll))
  ;;   (add-to-list 'keycast-substitute-alist `(,event nil)))

  ;; Those are for the `keycast-log-mode'
  (setq keycast-log-format "%-20K%C\n")
  (setq keycast-log-frame-alist
	'((minibuffer . nil)))
  (setq keycast-log-newest-first t))

(provide 'init-modeline)
;;; init-modeline.el ends here
