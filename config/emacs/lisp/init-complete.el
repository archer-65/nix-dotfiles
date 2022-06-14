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
  :init
  (vertico-mode)
  :setq
  (vertico-scroll-margin . 0)
  (vertico-count . 15)
  (vertico-cycle . t))


;; Cute centered child buffer
(leaf vertico-posframe
  :doc "Based on posframe library, for vertico"
  :ensure t
  :require t
  :init
  (vertico-posframe-mode 1)
  :config
  (defun vertico-posframe--show (buffer window-point)
    "`posframe-show' of vertico-posframe.
BUFFER will be showed by `posframe-show'.  After `posframe-show'
is called, window-point will be set to WINDOW-POINT."
    (let ((posframe
           ;; Some posframe poshandlers need infos of last-window.
           (with-selected-window (vertico-posframe-last-window)
             (apply #'posframe-show
                    buffer
                    :font vertico-posframe-font
                    :poshandler vertico-posframe-poshandler
                    :background-color (face-attribute 'vertico-posframe :background nil t)
                    :foreground-color (face-attribute 'vertico-posframe :foreground nil t)
                    :border-width vertico-posframe-border-width
                    :border-color (vertico-posframe--get-border-color)
                    :override-parameters vertico-posframe-parameters
                    :refposhandler vertico-posframe-refposhandler
                    :hidehandler #'vertico-posframe-hidehandler
                    :lines-truncate vertico-posframe-truncate-lines
                    (funcall vertico-posframe-size-function)))))
      ;; NOTE: `posframe-show' will force set window-point to 0, so we
      ;; need reset it again after `posframe-show'.
      (when (numberp window-point)
	(let ((window (frame-root-window posframe)))
          (when (window-live-p window)
            (set-window-point window window-point))))
      ;; NOTE: posframe will hide cursor, so we need let it show again.
      (with-current-buffer buffer
	(setq-local cursor-type 'bar)
	(setq-local cursor-in-non-selected-windows 'box))))
  (setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8))))

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
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(provide 'init-complete)
;;; init-complete.el ends here
