;;; init-packages.el --- Package manager and related configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file should be the `core`.
;; Here are initialized `straight.el` and `setup.el`.

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

(straight-use-package 'setup)
(require 'setup)

;; https://git.acdw.net/emacs/tree/lisp/+setup.el
(defun setup--straight-handle-arg (arg var)
  (cond
   ((and (boundp var) (symbol-value var)) t)
   ((keywordp arg) (set var t))
   ((functionp arg) (set var nil) (funcall arg))
   ((listp arg) (set var nil) arg)))

(setup-define :pkg
  (lambda (recipe &rest predicates)
    (let* ((skp (make-symbol "straight-keyword-p"))
           (straight-use-p
            (cl-mapcar
             (lambda (f) (setup--straight-handle-arg f skp))
             predicates))
           (form `(when ,@straight-use-p
                    (condition-case e
                        (straight-use-package ',recipe)
                      (error
                       ,(setup-quit))
                      (:success t)))))
      ;; Keyword arguments --- :quit is special and should short-circuit
      (if (memq :quit predicates)
          (setq form `,(setup-quit))
        ;; Otherwise, handle the rest of them ...
        (when-let ((after (cadr (memq :after predicates))))
          (setq form `(with-eval-after-load ,(if (eq after t)
                                                 (setup-get 'feature)
                                               after)
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
                 (or (car-safe recipe) recipe))))

(setup-define :face
  (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
  :documentation "Customize FACE to SPEC."
  :signature '(face spec ...)
  :debug '(setup)
  :repeatable t
  :after-loaded t)

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

(setup-define :disable
  'setup-quit
  :documentation "Always stop evaluating the body.")

(setup-define :hooks
  (lambda (hook func)
    `(add-hook ',hook #',func))
  :documentation "Add pairs of hooks."
  :repeatable t)

(setup-define :blackout
  (lambda (&optional mode)
    (setup (:pkg blackout))
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
