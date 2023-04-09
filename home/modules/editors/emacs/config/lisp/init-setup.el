;;; init-setup.el --- `setup.el' configuration -*- lexical-binding: t -*-

;;; Commentary:

;; The package `setup.el' is configured here, with new forms and settings.

;;; Code:

(straight-use-package 'setup)
(require 'setup)

;; Forms section

(setup-define :quit
  'setup-quit
  :documentation "Always stop evaluating the body.")

(setup-define :needs
  (lambda (executable)
    `(unless (executable-find ,executable)
       ,(setup-quit)))
  :documentation "If EXECUTABLE is not in the path, stop here."
  :repeatable 1)

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

(setup-define :hooks
  (lambda (hook func)
    `(add-hook ',hook #',func))
  :documentation "Add pairs of hooks."
  :repeatable t)

(setup-define :face
  (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
  :documentation "Customize FACE to SPEC."
  :signature '(face spec ...)
  :debug '(setup)
  :repeatable t
  :after-loaded t)

;; Blackout to hide minor modes
(straight-use-package 'blackout)
(setup-define :blackout
  (lambda (&optional mode)
    (let* ((mode (or mode (setup-get 'mode)))
           (mode (if (string-match-p "-mode\\'" (symbol-name mode))
                     mode
                   (intern (format "%s-mode" mode)))))
      `(blackout ',mode)))
  :documentation "Hide the mode-line lighter of the current mode with blackout.
MODE can be specified manually, and override the current-mode."
  :after-loaded t)

;; From https://git.acdw.net/emacs/tree/lisp/+setup.el
(defun +setup-warn (message &rest args)
  "Warn the user with that something bad happened in `setup'.
MESSAGE should be formatted (optionally) with ARGS"
  (display-warning 'setup (format message args)))

(defun +setup-wrap-to-demote-errors (body name)
  "Wrap BODY in a `with-demoted-errors' block.
This behavior is prevented if `setup-attributes' contains the
symbol `without-error-demotion'.

This function differs from `setup-wrap-to-demote-errors' in that
it includes the NAME of the setup form in the warning output."
  (if (memq 'without-error-demotion setup-attributes)
      body
    `(with-demoted-errors ,(format "Error in setup form on line %d (%s): %%S"
                                   (line-number-at-pos)
                                   name)
       ,body)))

(add-to-list 'setup-modifier-list '+setup-wrap-to-demote-errors)
(unless (memq debug-on-error '(nil init))
  (define-advice setup (:around (fn head &rest args) +setup-report)
    (+with-progress ((format "[Setup] %S..." head))
      (apply fn head args))))

;; Integration with `straight.el'
(defun setup--straight-handle-arg (arg var)
  (cond
   ((and (boundp var) (symbol-value var)) t)
   ((keywordp arg) (set var t))
   ((functionp arg) (set var nil) (funcall arg))
   ((listp arg) (set var nil) arg)))

(with-eval-after-load 'straight
  (setup-define :pkg
    (lambda (recipe &rest predicates)
      (let* ((skp (make-symbol "straight-keyword-p"))
             (straight-use-p (cl-mapcar
                              (lambda (f) (setup--straight-handle-arg f skp)) predicates))
             (form `(unless (and ,@straight-use-p
                                 (condition-case e (straight-use-package ',recipe)
                                   (error (+setup-warn ":straight error: %S" ',recipe)
                                          ,(setup-quit))
                                   (:success t)))
                      ,(setup-quit))))
        ;; Keyword arguments --- :quit is special and should short-circuit
        (if (memq :quit predicates)
            (setq form `,(setup-quit))
          ;; Otherwise, handle the rest of them ...
          (when-let ((after (cadr (memq :after predicates))))
            (setq form `(with-eval-after-load ,(if (eq after t) (setup-get 'feature) after)
                          ,form))))
        ;; Finally ...
        form))
    :documentation "Install RECIPE with `straight-use-package'.
If PREDICATES are given, only install RECIPE if all of them return non-nil.
The following keyword arguments are also recognized:
- :quit          --- immediately stop evaluating.  Good for commenting.
- :after FEATURE --- only install RECIPE after FEATURE is loaded.
                     If FEATURE is t, install RECIPE after the current feature."
    :repeatable nil
    :indent 1
    :shorthand (lambda (sexp)
                 (let ((recipe (cadr sexp)))
                   (or (car-safe recipe) recipe)))))

(provide 'init-setup)
;;; init-setup.el ends here
