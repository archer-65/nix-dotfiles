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
(defun archer-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher, using equal sign as a suffix."
  (cond
   ((equal "=" pattern)
    '(orderless-literal . "="))
   ((string-suffix-p "=" pattern)
    (cons 'orderless-literal (substring pattern 0 -1)))))

(defun archer-orderless-without-literal-dispatcher (pattern _index _total)
  "Literal without style dispatcher using the exclamation mark as a suffix."
  (cond
   ((equal "!" pattern)
    '(orderless-literal . "!"))
   ((string-suffix-p "!" pattern)
    (cons 'orderless-without-literal (substring pattern 0 -1)))))

(defun archer-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism dispatcher using comma as suffix."
  (cond
   ((equal "," pattern)
    '(orderless-literal . ","))
   ((string-suffix-p "," pattern)
    (cons 'orderless-initialism (substring pattern 0 -1)))))

(defun archer-orderless-flex-dispatcher (pattern _index _total)
  "Flex dispatcher using the tilde suffix."
  (cond
   ((equal "~" pattern)
    '(orderless-literal . "~"))
   ((string-suffix-p "~" pattern)
    (cons 'orderless-flex (substring pattern 0 -1)))))

(leaf orderless
  :doc "Orderless completion style for your Completion UI/Framework"
  :straight t
  :init
  (setq completion-styles '(orderless basic)
	orderless-component-separator 'orderless-escapable-split-on-space
        completion-category-defaults nil)

  (setq orderless-style-dispatchers
	'(archer-orderless-literal-dispatcher
	  archer-orderless-without-literal-dispatcher
	  archer-orderless-initialism-dispatcher
	  archer-orderless-flex-dispatcher))

  (setq completion-category-overrides
	'((file (styles . (partial-completion basic orderless)))
	  (project-file (styles . (partial-completion basic orderless))))))

(provide 'init-complete)
;;; init-complete.el ends here
