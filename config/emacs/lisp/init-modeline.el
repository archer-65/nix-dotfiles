;;; init-modeline.el --- Modeline customization -*- lexical-binding: t -*-

;;; Commentary:

;; Modeline customization and other useless/cute packages.

;;; Code:

(unless (version< emacs-version "28")
  (setq mode-line-compact nil))

;; (defun archer--modeline-buffer-state ()
;;   "Define `modified' state icon."
;;   (let* ((config-alist
;; 	 '(("*" all-the-icons-faicon-family all-the-icons-faicon "circle" :height 1.3 :v-adjust -0.0)
;; 	   ("-" all-the-icons-faicon-family all-the-icons-faicon "circle-o" :height 1.3 :v-adjust -0.0)
;; 	   ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust -0.05)))
;;     (result (cdr (assoc (format-mode-line "%*") config-alist))))
;;   (propertize (apply (cadr result) (cddr result))
;; 	      'face `(:family ,(funcall (car result))))))

;; (defun archer--modeline-mode-icon ()
;;   "Segment defining the icon based on major mode."
;;   (let ((icon (all-the-icons-icon-for-mode major-mode)))
;;     (propertize icon
;; 		'help-echo (format "Major mode: `%s`" major-mode)
;; 		'face `(:height 1.05
;; 			:v-adjust -0.0
;; 			:family ,(all-the-icons-icon-family-for-mode major-mode)))))

;; (setq-default mode-line-format
;;               (list

;; 	       mode-line-front-space
	       
;;                " "

;; 	       '(:eval (archer--modeline-buffer-state))

;; 	       "  "

;; 	       ;; '(:eval (archer--modeline-mode-icon))

;; 	       ;; " "

;; 	       mode-line-buffer-identification

;; 	       "  "

;;                mode-line-position

;;                '(pdf-misc-size-indication-minor-mode
;;                  (:eval (let* ((page (pdf-view-current-page))
;;                                (pdf-page (nth (1- page) (pdf-cache-pagelabels))))
;;                           (list
;;                            " "
;;                            (when (not (string= (number-to-string page) pdf-page))
;;                              (list "(" pdf-page ") "))
;;                            (number-to-string (pdf-view-current-page))
;;                            "/"
;;                            (number-to-string (pdf-cache-number-of-pages))))))
	       
;; 	       mode-line-modes

;; 	       "  "
	       
;; 	       '(:eval (when-let (vc vc-mode)
;;                          (list (all-the-icons-alltheicon "git" :v-adjust -0.0)
;; 			       " "
;;                                (propertize (substring vc 5)
;;                                            'face 'font-lock-comment-face)
;;                                " ")))

;;                mode-line-misc-info
;; 	       mode-line-end-spaces
;;                ))

;; (setq mode-line-position-column-line-format '("<%l:%c>") ; Emacs 28
;;       mode-line-defining-kbd-macro (propertize " Macro" 'face 'mode-line-emphasis))

(leaf doom-modeline
  :doc "A very attractive and rich (yet still minimal) mode line configuration for Emacs."
  :straight t
  ;:disabled t
  :hook
  (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width . 1)
  (doom-modeline-height . 30)
  (doom-modeline-minor-modes . nil)
  (doom-modeline-major-mode-icon . t)
  (doom-modeline-major-mode-color-icon . t)
  (doom-modeline-buffer-file-state-icon . t)
  (doom-modeline-buffer-file-name-style . 'truncate-upto-project))

;; Hiding minor mode can be useful, but right now I have disabled this behavior.
(leaf minions
  :doc "Hide minor modes"
  :straight t
  :disabled t
  :config
  (minions-mode 1))

(provide 'init-modeline)
;;; init-modeline.el ends here
