;;; init-complete-in-buffer.el --- In buffer completion configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Corfu/Company and extra Dabbrev configuration if needed.

;;; Code:

(leaf company
  :straight t
  :bind
  (:lsp-mode-map
   ("<tab>" . company-indent-or-complete-common))
  (:company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-s" . company-filter-candidates)
   ("C-i" . company-complete-selection)
   ("<tab>" . company-complete-selection)
   ("M-d" . company-show-doc-buffer))
  (:company-search-map ("C-n" . company-select-next)
                       ("C-p" . company-select-previous))
  ("C-c C-/" . company-files)
  ("C-c y" . company-yasnippet)
  :hook
  (telega-chat-mode-hook . company-mode)
  :custom
  (global-company-mode . t))

(leaf corfu
  :disabled t
  :straight t
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)

  ;; Function to enable corfu in lsp-mode
  (defun archer/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure flex

  ;; Load and enable corfu-history
  (load "extensions/corfu-history")
  (corfu-history-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold t)
  :custom
  (corfu-cycle . t)                 ;; Enable cycling for `corfu-next/previous'
  (corfu-auto . t)                  ;; Enable auto completion
  (corfu-separator . ?\s)           ;; Orderless field separator
  (corfu-quit-at-boundary . nil)    ;; Never quit at completion boundary
  (corfu-quit-no-match . nil)       ;; Never quit, even if there is no match
  (corfu-preview-current . nil)     ;; Disable current candidate preview
  (corfu-preselect-first . nil)     ;; Disable candidate preselection
  (corfu-on-exact-match . nil)      ;; Configure handling of exact matches
  (corfu-echo-documentation . 0.25) ;; Disable documentation in the echo area
  (corfu-scroll-margin . 5)         ;; Use scroll margin

  ;; Mandatory for LSP completion with Corfu
  (lsp-completion-provider . :none)

  :hook
  (lsp-completion-mode . archer/lsp-mode-setup-completion))

(provide 'init-complete-in-buffer)
;;; init-complete-in-buffer.el ends here
