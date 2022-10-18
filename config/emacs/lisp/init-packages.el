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

(provide 'init-packages)
;;; init-packages.el ends here
