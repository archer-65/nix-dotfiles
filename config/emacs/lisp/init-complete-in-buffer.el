;;; init-complete-in-buffer.el --- In buffer completion configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Corfu/Company and extra Dabbrev configuration if needed.

;;; Code:

(leaf company
  ;; :after lsp-mode
  :straight t
  :bind
  (company-active-map
   ("<tab>" . company-complete-selection))
  :hook
  (telega-chat-mode-hook . company-mode))

(leaf corfu
  :straight t
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

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)

  ;; Load and enable corfu-history
  (load "extensions/corfu-history")
  (corfu-history-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold t))

(provide 'init-complete-in-buffer)
;;; init-complete-in-buffer.el ends here
