;;; init-packages.el --- Package manager and related configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file should be the `core`.
;; Here are initialized `straight.el` and `leaf`.

;;; Code:

;;; SECTION STRAIGHT.EL
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;; SECTION STRAIGHT.EL ENDS HERE

;;; SECTION LEAF INSTALLATION
(eval-and-compile
  (straight-use-package 'leaf)
  (straight-use-package 'leaf-keywords)
  (straight-use-package 'blackout)

  (leaf leaf-keywords
    (straight-use-package 'leaf-keywords)
    :init
    (leaf blackout (straight-use-package 'blackout))
    :config
    (leaf-keywords-init)))
;;; SECTION LEAF INSTALLATION ENDS HERE

(straight-use-package 'setup)
(require 'setup)

(setup-define :straight
  (lambda (recipe)
    `(unless (straight-use-package ',recipe)
       ,(setup-quit)))
  :documentation
  "Install RECIPE with `straight-use-package`.
This macro can be used as HEAD, and will replace itself with the first RECIPE's package.'"
  :repeatable t
  :shorthand (lambda (sexp)
               (let ((recipe (cadr sexp)))
                 (if (consp recipe)
                     (car recipe)
                   recipe))))

(setup-define :straight-unless
  (lambda (recipe condition)
    `(unless ,condition
       (straight-use-package ',recipe)
       ,(setup-quit)))
  :documentation
  "Install RECIPE with `straight-use-package' unless CONDITION is met.
If CONDITION is true, stop evaluating the body.  This macro can
be used as HEAD, and will replace itself with the RECIPE's
package.  This macro is not repeatable."
  :repeatable nil
  :indent 1
  :shorthand (lambda (sexp)
               (let ((recipe (cadr sexp)))
                 (if (consp recipe) (car recipe) recipe))))

(setup-define :doc
  (lambda (&rest _) nil)
  :documentation "The one line doc for the setup package.")

(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t
  :signature '(FUNC ...))

(setup-define :load-after
  (lambda (features &rest body)
    (let ((body `(progn
                   (require ',(setup-get 'feature))
                   ,@body)))
      (dolist (feature (nreverse (ensure-list features)))
	(setq body `(with-eval-after-load ',feature ,body)))
      body))
  :indent 1
  :documentation "Load the current feature after FEATURES.")

(setup-define :with-after
  (lambda (features &rest body)
    (let ((body `(progn ,@body)))
      (dolist (feature (nreverse (ensure-list features)))
	(setq body `(with-eval-after-load ',feature ,body)))
      body))
  :indent 1
  :documentation "Evaluate BODY after FEATURES are loaded.")

(setup-define :disabled
  (lambda ()
    `,(setup-quit))
  :documentation "Always stop evaluating the body.")

(setup-define :hooks
  (lambda (hook func)
    `(add-hook ',hook #',func))
  :documentation "Add pairs of hooks."
  :repeatable t)

(setup-define :blackout
  (lambda (&optional mode)
    (setup (:straight blackout))
    (let* ((mode (or mode (setup-get 'mode)))
	   (mode (if (string-match-p "-mode\\'" (symbol-name mode))
		     mode
		   (intern (format "%s-mode" mode)))))
      `(blackout ',mode)))
  :documentation "Hide the mode-line lighter of the current mode with blackout.
MODE can be specified manually, and override the current-mode."
  :after-loaded t)

(provide 'init-packages)
;;; init-packages.el ends here
