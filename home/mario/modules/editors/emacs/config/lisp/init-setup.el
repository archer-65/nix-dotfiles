;;; init-setup.el --- `setup.el' configuration -*- lexical-binding: t -*-

;;; Commentary:

;; The package `setup.el' is configured here, with new forms and settings.

;;; Code:

(elpaca setup (require 'setup))
(elpaca-wait)

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

;; Forms section
(setup-define :quit
  #'setup-quit
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
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :with-after
  (lambda (features &rest body)
    (let ((body `(progn ,@body)))
      (dolist (feature (nreverse (ensure-list features)))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :indent 1
  :documentation "Evaluate BODY after FEATURES are loaded.")

(setup-define :face
  (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
  :documentation "Customize FACE to SPEC."
  :signature '(face spec ...)
  :debug '(setup)
  :repeatable t
  :after-loaded t)

(setup-define :hide-mode
  (lambda (&optional mode)
    (let* ((mode (or mode (setup-get 'mode)))
           (mode (if (string-match-p "-mode\\'" (symbol-name mode))
                     mode
                   (intern (format "%s-mode" mode)))))
      `(setq minor-mode-alist
             (delq (assq ',mode minor-mode-alist)
                   minor-mode-alist))))
  :documentation "Hide the mode-line lighter of the current mode.
Alternatively, MODE can be specified manually, and override the
current mode."
  :after-loaded t)

;;;###autoload
(defmacro setup-pkg (order &rest body)
  "Execute BODY in `setup' declaration after ORDER is finished.
If the :disabled keyword is present in body, the package is completely ignored.
This happens regardless of the value associated with :disabled.
The expansion is a string indicating the package has been disabled."
  (declare (indent 1))
  (if (memq :disabled body)
      (format "%S :disabled by setup-pkg" order)
    (let ((o order))
      (when-let ((ensure (cl-position :ensure body)))
        (setq o (if (null (nth (1+ ensure) body)) nil order)
              body (append (cl-subseq body 0 ensure)
                           (cl-subseq body (+ ensure 2)))))
      `(elpaca ,o (setup
                    ,(if-let (((memq (car-safe order) '(quote \`)))
                              (feature (flatten-tree order)))
                         (cadr feature)
                       (elpaca--first order))
                    ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Another try... Elpaca damn you!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-wrap-to-install-package (body _name)
"Wrap BODY in an `elpaca' block if necessary.
The body is wrapped in an `elpaca' block if `setup-attributes'
contains an alist with the key `elpaca'."
(if (assq 'elpaca setup-attributes)
    `(elpaca ,(cdr (assq 'elpaca setup-attributes)) ,@(macroexp-unprogn body))
  body))

;; Add the wrapper function
(add-to-list 'setup-modifier-list #'setup-wrap-to-install-package t)

(setup-define :elpaca
  (lambda (order-or-recipe &optional recipe-list)
    (push (cond
           ;; Handle (:elpaca t) - simple package installation
           ((eq order-or-recipe t)
            `(elpaca . ,(setup-get 'feature)))
           ;; Handle (:elpaca package-name (:host "github.com" ...)) - explicit package with recipe
           ((and recipe-list (listp recipe-list) (keywordp (car recipe-list)))
            `(elpaca . (,order-or-recipe ,@recipe-list)))
           ;; Handle (:elpaca (:host "github.com" ...)) - recipe as list, use feature name
           ((and (listp order-or-recipe) (keywordp (car order-or-recipe)))
            `(elpaca . (,(setup-get 'feature) ,@order-or-recipe)))
           ;; Handle (:elpaca package-name) - explicit package name only
           (t
            `(elpaca . ,order-or-recipe)))
          setup-attributes)
    nil)
  :documentation "Install package with `elpaca'.
ORDER-OR-RECIPE can be:
- t: Install package with same name as feature
- A list starting with keyword: Recipe properties like (:host \"github.com\" :repo \"user/repo\")
- A symbol/string: Explicit package name
When RECIPE-LIST is provided, ORDER-OR-RECIPE is treated as the package name."
  :shorthand #'cadr)

(defun setup-wrap-to-disable-conditionally (body _name)
  "Conditionally disable setup form based on `disable' attribute.
If `setup-attributes' contains an alist with the key `disable',
wrap the body in a conditional that evaluates at runtime.
If no disable condition is present, return BODY unchanged."
  (if-let ((disable-condition (cdr (assq 'disable setup-attributes))))
      `(unless ,disable-condition ,body)
    body))

;; Add the disable wrapper function (append to run after elpaca and others)
(add-to-list 'setup-modifier-list #'setup-wrap-to-disable-conditionally t)

(setup-define :disable
  (lambda (condition)
    (push `(disable . ,condition) setup-attributes)
    nil)
  :documentation "Conditionally disable the setup form.
If CONDITION evaluates to non-nil, the entire setup form is disabled.")

(provide 'init-setup)
;;; init-setup.el ends here
