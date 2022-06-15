;;; init-embark.el --- Embark, run a command based on point-*- lexical-binding: t -*-

;;; Commentary:

;; Sometimes you want to act near point, but there are many actions.
;; Embark ships many actions, dependant on target and modes.

;;; Code:

(defun archer/embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
  '(archer/embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun archer/embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'archer/embark-which-key-indicator embark-indicators)))
      (apply fn args)))

;; Embark configuration
(leaf embark
  :doc "Act near point :D"
  :straight t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (advice-add #'embark-completing-read-prompter :around #'archer/embark-hide-which-key-indicator)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Used for export and edit after ripgrep magic.
(leaf wgrep
  :doc "Edit matches in place."
  :after embark
  :straight t)

;; Integration with Consult
(leaf embark-consult
  :straight t
  :after (embark consult)
  :require t
  :leaf-defer nil ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-embark)
;;; init-embark.el ends here
