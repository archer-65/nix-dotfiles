;;; init-modeline.el --- Modeline customization -*- lexical-binding: t -*-

;;; Commentary:

;; Modeline customization and other useless/cute packages.

;;; Code:
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
		mode-line-end-spaces))

(leaf recursion-indicator
  :straight t
  :init
  ;; <https://github.com/minad/recursion-indicator>.
  (setq-default mode-line-modes
		(seq-filter (lambda (s)
			      (not (and (stringp s)
					(string-match-p
					 "^\\(%\\[\\|%\\]\\)$" s))))
                            mode-line-modes))
  :config
  (setq recursion-indicator-general (concat "general" (all-the-icons-material "cached" :v-adjust -0.1)))
  (setq recursion-indicator-minibuffer (concat "minibuffer " (all-the-icons-material "cached" :v-adjust -0.1)))
  (recursion-indicator-mode 1))

;;; Keycast mode
(leaf keycast
  :straight t
  :config
  ;; Those are for `keycast-mode'
  (setq keycast-mode-line-window-predicate #'keycast-active-frame-bottom-right-p)
  (setq keycast-separator-width 1)
  (setq keycast-mode-line-remove-tail-elements nil)
  (setq keycast-mode-line-format "%3s%k%c%r")

  ;; Based on Prot's configuration
  (dolist (input '(self-insert-command
		   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p
		   mouse-movement-p
		   mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil)))

  ;; Those are for the `keycast-log-mode'
  (setq keycast-log-format "%-20K%C\n")
  (setq keycast-log-frame-alist
	'((minibuffer . nil)))
  (setq keycast-log-newest-first t))

;; Hiding minor mode can be useful, but right now I have disabled this behavior.
(leaf minions
  :doc "Hide minor modes"
  :straight t
  :disabled t
  :config
  (minions-mode 1))

(leaf doom-modeline
  :doc "A very attractive and rich (yet still minimal) mode line configuration for Emacs."
  :straight t
  :disabled t
  :hook
  (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width . 1)
  (doom-modeline-height . 25)
  (doom-modeline-minor-modes . nil)
  (doom-modeline-major-mode-icon . t)
  (doom-modeline-major-mode-color-icon . t)
  (doom-modeline-buffer-file-state-icon . t)
  (doom-modeline-buffer-file-name-style . 'truncate-upto-project))

(provide 'init-modeline)
;;; init-modeline.el ends here
