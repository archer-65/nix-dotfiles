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

;; Copyright - Amazon Q CLI :D
(defun +setup-add-modifier (modifier &optional position relative-to)
  "Smartly add MODIFIER to `setup-modifier-list'.
POSITION can be:
- nil or 'append: Add to end (default)
- 'prepend: Add to beginning
- 'before: Add before RELATIVE-TO function
- 'after: Add after RELATIVE-TO function

If MODIFIER already exists, remove it first to avoid duplicates."
  (setq setup-modifier-list (delq modifier setup-modifier-list))
  (pcase position
    ('prepend
     (push modifier setup-modifier-list))
    ('before
     (if-let ((pos (cl-position relative-to setup-modifier-list)))
         (setq setup-modifier-list
               (append (cl-subseq setup-modifier-list 0 pos)
                       (list modifier)
                       (cl-subseq setup-modifier-list pos)))
       (push modifier setup-modifier-list)))
    ('after
     (if-let ((pos (cl-position relative-to setup-modifier-list)))
         (setq setup-modifier-list
               (append (cl-subseq setup-modifier-list 0 (1+ pos))
                       (list modifier)
                       (cl-subseq setup-modifier-list (1+ pos))))
       (setq setup-modifier-list (append setup-modifier-list (list modifier)))))
    (_
     (setq setup-modifier-list (append setup-modifier-list (list modifier))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elpaca integration with setup.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-wrap-to-install-package (body _name)
  "Wrap BODY in an `elpaca' block if necessary.
The body is wrapped in an `elpaca' block if `setup-attributes'
contains an alist with the key `elpaca'. If there's also post-install
code, it gets added to the elpaca body."
  (if (assq 'elpaca setup-attributes)
      (let ((elpaca-spec (cdr (assq 'elpaca setup-attributes)))
            (post-install (cdr (assq 'post-install setup-attributes))))
        `(elpaca ,elpaca-spec
           ,@(macroexp-unprogn body)
           ,@(when post-install (list post-install))))
    body))

;; Add the elpaca wrapper function
(+setup-add-modifier #'setup-wrap-to-install-package 'append)

(setup-define :pkg
  (lambda (order-or-recipe &optional recipe-list)
    (let ((should-install t)
          (final-order nil)
          (final-recipe nil)
          (post-install-code nil))

      ;; Parse arguments and determine installation condition
      (cond
       ;; Handle (:pkg t) - simple package installation
       ((eq order-or-recipe t)
        (setq final-order (setup-get 'feature)))

       ;; Handle (:pkg package-name (:host "github.com" ...)) - explicit package with recipe
       ((and recipe-list (listp recipe-list) (keywordp (car recipe-list)))
        (setq final-order order-or-recipe
              final-recipe recipe-list))

       ;; Handle (:pkg (:host "github.com" ...)) - recipe as list, use feature name
       ((and (listp order-or-recipe) (keywordp (car order-or-recipe)))
        (setq final-order (setup-get 'feature)
              final-recipe order-or-recipe))

       ;; Handle (:pkg package-name) - explicit package name only
       (t
        (setq final-order order-or-recipe)))

      ;; Check for conditional installation and extract post-install code
      (when final-recipe
        ;; Extract :post-install code before any other processing
        (when-let ((post-install (plist-get final-recipe :post-install)))
          (setq post-install-code post-install))

        ;; Check for :nix property - if present and we're using nix, don't install
        (when (and (plist-get final-recipe :nix)
                   (fboundp 'archer-using-nix-p)
                   (archer-using-nix-p))
          (setq should-install nil)
          ;; Add to elpaca-ignored-dependencies to prevent downloading as dependency
          (when (boundp 'elpaca-ignored-dependencies)
            (cl-pushnew final-order elpaca-ignored-dependencies)))

        ;; Remove our custom properties from the recipe before passing to elpaca
        (setq final-recipe (cl-loop for (key value) on final-recipe by #'cddr
                                    unless (memq key '(:nix :post-install))
                                    collect key and collect value)))

      ;; Push elpaca attribute if we should install
      (when should-install
        (push (if final-recipe
                  `(elpaca . (,final-order ,@final-recipe))
                `(elpaca . ,final-order))
              setup-attributes))
      
      ;; Push post-install code to attributes if present and we should install
      (when (and post-install-code should-install)
        (push `(post-install . (with-eval-after-load ',(setup-get 'feature)
                                 ,post-install-code))
              setup-attributes))
      nil))
  :documentation "Install package with `elpaca', with optional Nix-aware conditional installation.

This keyword supports multiple forms:

1. Simple installation using feature name:
   (:pkg t)
   → (elpaca feature-name ...)

2. Recipe as plist (inferred package name):
   (:pkg (:host \"github.com\" :repo \"user/repo\"))
   → (elpaca (feature-name :host \"github.com\" :repo \"user/repo\") ...)

3. Explicit package name:
   (:pkg different-package-name)
   → (elpaca different-package-name ...)

4. Explicit package name with recipe:
   (:pkg package-name (:host \"github.com\" :repo \"user/repo\"))
   → (elpaca (package-name :host \"github.com\" :repo \"user/repo\") ...)

NIX-AWARE INSTALLATION:
Add :nix t to skip installation when using Nix.
NOTE: This could change to be a more flexible alternative. Say :type, or :prefer.

5. Skip installation when using Nix:
   (:pkg (:nix t))
   → Only installs if NOT using Nix

6. Combined with regular recipe properties:
   (:pkg (:host \"github.com\" :repo \"user/repo\" :nix t))
   → Recipe with Nix-aware conditional installation

POST-INSTALLATION SETUP:
Add :post-install to run code after package installation:
NOTE: This could change to be a more flexible alternative.

7. Post-installation setup (only when installed via elpaca):
   (:pkg (:nix t :post-install (pdf-tools-install :no-query)))
   → Runs post-install code only when elpaca installs the package

8. Combined example:
   (:pkg (:host \"github.com\" :repo \"user/repo\" :nix t :post-install (setup-function)))

The special properties :nix and :post-install are removed before passing to elpaca.
Use :disable for other conditional installation scenarios."
  :shorthand #'cadr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditional disabling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-wrap-to-disable-conditionally (body _name)
  "Conditionally disable setup form based on `disable' attribute.
If `setup-attributes' contains an alist with the key `disable',
wrap the entire BODY in a conditional that evaluates at runtime.
If no disable condition is present, return BODY unchanged.

This wrapper should run after other wrappers (like elpaca) to ensure
the entire form gets conditionally disabled."
  (if-let ((disable-condition (cdr (assq 'disable setup-attributes))))
      `(unless ,disable-condition ,body)
    body))

;; Add the disable wrapper function (after elpaca wrapper)
(+setup-add-modifier #'setup-wrap-to-disable-conditionally 'after #'setup-wrap-to-install-package)

(setup-define :disable
  (lambda (condition)
    (push `(disable . ,condition) setup-attributes)
    nil)
  :documentation "Conditionally disable the entire setup form.

CONDITION is evaluated at runtime. If it evaluates to non-nil,
the entire setup form (including package installation) is skipped.

Examples:
  (:disable t)                              ; Always disabled
  (:disable (eq system-type 'windows-nt))   ; Disabled on Windows
  (:disable (not (executable-find \"git\")))  ; Disabled if git not found
  (:disable (version< emacs-version \"29\"))  ; Disabled on old Emacs

This is useful for:
- Platform-specific packages
- Packages requiring external dependencies
- Experimental configurations
- Version-specific features")

(provide 'init-setup)
;;; init-setup.el ends here
