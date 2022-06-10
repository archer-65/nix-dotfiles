;;; packages.el --- Package manager and related configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file should be the `core`.
;; Here are initialized `package.el/straight.el` and `use-package/leaf`

;;; Code:

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

;;; SECTION USE-PACKAGE TO DEPRECATE
;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;;; SECTION USE-PACKAGE TO DEPRECATE ENDS HERE

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
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf leaf-convert :ensure t)
;;; SECTION LEAF INSTALLATION ENDS HERE

;; Auto updates
(leaf auto-package-update
  :doc "Auto update packages (only works for package.el)"
  :custom
  (auto-package-update-interval . 7)
  (auto-package-update-prompt-before-update . t)
  (auto-package-update-hide-results . t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "16:00"))

(provide 'packages)
;;; packages.el ends here
