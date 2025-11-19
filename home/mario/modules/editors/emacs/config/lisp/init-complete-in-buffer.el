;;; init-complete-in-buffer.el --- In buffer completion configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Corfu completion UI and Cape extensions for better completion at point.

;;; Code:

(setup corfu
  (:pkg (:files (:defaults "extensions/*")
                   :includes (corfu-history corfu-popupinfo corfu-info)))
  (setopt corfu-cycle t
           corfu-auto t
           corfu-auto-delay 0.3
           corfu-auto-prefix 3
           corfu-separator ?\s
           corfu-quit-at-boundary 'separator
           corfu-quit-no-match 'separator
           corfu-preview-current #'insert
           corfu-preselect-first t
           corfu-preselect 'valid
           corfu-on-exact-match #'insert
           corfu-echo-documentation 0.25
           corfu-min-width 30
           corfu-scroll-margin 5)

  (:with-map corfu-map
    (:bind
      "C-n"      corfu-next
      [tab]      corfu-next
      "C-p"      corfu-previous
      [backtab]  corfu-previous
      "<escape>" corfu-quit
      "<return>" corfu-insert
      "M-SPC"    corfu-insert-separator))

  ;; Cute extras ;)
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

  (defun contrib-corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  (:with-map corfu-map
    (:bind
      [remap move-beginning-of-line] #'corfu-beginning-of-prompt
      [remap move-end-of-line] #'corfu-end-of-prompt
      "M-m" contrib-corfu-move-to-minibuffer))

  ;; Found in Prot's configuration
  (defun contrib-corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (or (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (:with-hook minibuffer-setup-hook
    (:hook contrib-corfu-enable-always-in-minibuffer))

  ;; TODO: This is really buggy, to inspect.
  (remove-hook 'completion-at-point-functions
               #'tags-completion-at-point-function)

  (global-corfu-mode))

(setup corfu-history
  (:autoload corfu-history-mode)
  (:with-after (corfu savehist)
    (add-to-list 'savehist-additional-variables 'corfu-history)
    (corfu-history-mode 1)))

(setup corfu-popupinfo
  (:autoload corfu-popupinfo-mode)
  (:with-after corfu
    (setopt corfu-popupinfo-delay '(0.5 . 0))
    (corfu-popupinfo-mode 1)
    (:with-map corfu-popupinfo-map
      (:bind
       "M-p" corfu-popupinfo-scroll-down
       "M-n" corfu-popupinfo-scroll-up
       "M-d" corfu-popupinfo-toggle))))

(setup corfu-info
  (:load-after corfu)
  (:with-map corfu-map
    (:bind
      "M-d" corfu-info-documentation
      "M-l" corfu-info-location)))

(setup kind-icon
  (:pkg t)
  (:with-after corfu
    (setopt kind-icon-default-face 'corfu-default
             kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.7 :scale 1.0))
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(setup cape
  (:pkg t)
  ;; Needed for company-backends!
  (setup company
  (:pkg t)
    (:autoload company-grab))

  (dolist (backend '(cape-elisp-symbol cape-keyword cape-file cape-history cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend))

  (keymap-global-set "C-c p p"  'completion-at-point)
  (keymap-global-set "C-c p t"  'complete-tag)
  (keymap-global-set "C-c p d"  'cape-dabbrev)
  (keymap-global-set "C-c p h"  'cape-history)
  (keymap-global-set "C-c p f"  'cape-file)
  (keymap-global-set "C-c p k"  'cape-keyword)
  (keymap-global-set "C-c p s"  'cape-elisp-symbol)
  (keymap-global-set "C-c p e"  'cape-elisp-block)
  (keymap-global-set "C-c p a"  'cape-abbrev)
  (keymap-global-set "C-c p i"  'cape-ispell)
  (keymap-global-set "C-c p l"  'cape-line)
  (keymap-global-set "C-c p w"  'cape-dict)
  (keymap-global-set "C-c p :"  'cape-emoji)
  (keymap-global-set "C-c p \\" 'cape-tex)
  (keymap-global-set "C-c p _"  'cape-tex)
  (keymap-global-set "C-c p ^"  'cape-tex)
  (keymap-global-set "C-c p &"  'cape-sgml)
  (keymap-global-set "C-c p r"  'cape-rfc1345))

(provide 'init-complete-in-buffer)
;;; init-complete-in-buffer.el ends here
