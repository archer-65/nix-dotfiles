;;; init-packages.el --- Package manager and related configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file should be the `core`.
;; Here are initialized `package.el/straight.el` and `use-package/leaf`.

;;; Code:

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

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

  (leaf leaf-keywords
    (straight-use-package 'leaf-keywords)
    :init
    (leaf straight (straight-use-package 'straight))
    (leaf blackout (straight-use-package 'blackout))
    ;:custom
    ;(leaf-alias-keyword-alist .'((:ensure . :straight)))
    :config
    (leaf-keywords-init)))

(leaf leaf-convert :straight t)
;;; SECTION LEAF INSTALLATION ENDS HERE

(provide 'init-packages)
;;; init-packages.el ends here
