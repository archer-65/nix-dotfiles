;;; init-complete.el --- Completion enhancement -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs' internal completion is awesome, why should you use Ivy/Helm and similar?
;; They're wonderful, but complex and for me are unnecessary.
;; I'm using Vertico, Orderless and Marginalia (monster trio) for rich, orderless completion style.

;;; Code:

;; Vertico
(leaf vertico
  :doc "Performant and minimalistic vertical completion
        UI based on the default completion system."
  :straight t
  :init
  (vertico-mode)
  :setq
  (vertico-scroll-margin . 0)
  (vertico-count . 15)
  (vertico-cycle . t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(leaf savehist
  :init
  (savehist-mode))

;; Other useful settings
(leaf emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Marginalia
(leaf marginalia
  :doc "Annotations placed at the margin of the minibuffer for completion candidates."
  :straight t
  ;; Enable `marginalia-cycle` globally and in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         (minibuffer-local-map
          ("M-A" . marginalia-cycle)))
  :init
  ;; Must be in the :init such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Orderless
(leaf orderless
  :doc "Orderless completion style for your Completion UI/Framework"
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(provide 'init-complete)
;;; init-complete.el ends here
