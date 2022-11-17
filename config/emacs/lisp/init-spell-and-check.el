;;; init-spell-and-check.el --- Spell and syntax checking based on modes -*- lexical-binding: t -*-

;;; Commentary:

;; Flyspell as spell checker, while Flycheck as syntax checker for prog-mode.

;;; Code:

;; (leaf flymake
;;   :config
;;   (setq flymake-fringe-indicator-position 'left-fringe)
;;   (setq flymake-suppress-zero-counters t)
;;   (setq flymake-start-on-flymake-mode t)
;;   (setq flymake-no-changes-timeout 0.5)
;;   (setq flymake-start-on-save-buffer t)
;;   (setq flymake-proc-compilation-prevents-syntax-check t)
;;   (setq flymake-wrap-around nil)

;;   (setq flymake-mode-line-format
;;         '("" flymake-mode-line-exception flymake-mode-line-counters))

;;   (setq flymake-mode-line-counter-format
;;         '(" " flymake-mode-line-error-counter
;;           flymake-mode-line-warning-counter
;;           flymake-mode-line-note-counter ""))

;;   (define-key ctl-x-x-map "m" #'flymake-mode) ; C-x x m
;;   (let ((map flymake-mode-map))
;;     (define-key map (kbd "C-c ! s") #'flymake-start)
;;     (define-key map (kbd "C-c ! d") #'flymake-show-buffer-diagnostics) ; Emacs28
;;     (define-key map (kbd "C-c ! D") #'flymake-show-project-diagnostics) ; Emacs28
;;     (define-key map (kbd "C-c ! n") #'flymake-goto-next-error)
;;     (define-key map (kbd "C-c ! p") #'flymake-goto-prev-error))

;;   (add-hook 'prog-mode-hook 'flymake-mode)
;;   (add-hook 'text-mode-hook 'flymake-mode))

;; ;; From Purcell's dotfiles
;; (leaf flymake-flycheck
;;   :straight t
;;   :after flymake)

(leaf flycheck
  :straight t
  :commands (flycheck-list-errors flycheck-buffer)
  :config
  ;; HACK: without this, the keywords added by leaf-keywords don't
  ;; take effect in the checker process, so they all trigger an
  ;; "unrecognized keyword" error.
  (setq flycheck-emacs-lisp-check-form
	(if (string-match-p "leaf-keywords"
                            flycheck-emacs-lisp-check-form)
            ;; Don't do anything on subsequent evals
            flycheck-emacs-lisp-check-form
          (format
           "(progn %s %s)"
           '(progn
              (require 'leaf-keywords nil t)
              (leaf-keywords-init))
           flycheck-emacs-lisp-check-form)))
  (global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path . 'inherit)
  (flycheck-idle-change-delay . 1.0)
  (flycheck-display-errors-delay . 0.25)
  (flycheck-emacs-lisp-initialize-packages . t))

(leaf flyspell
  :hook
  (text-mode-hook . (lambda () flyspell-mode 1))
  (prog-mode-hook . flyspell-prog-mode))

(provide 'init-spell-and-check)
;;; init-spell-and-check.el ends here
