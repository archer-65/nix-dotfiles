;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:

;; Starting with Emacs 27, the default behaviour is
;; to start the package manager before loading the init file.
;; This file loads basic stuff before GUI is initialized.

;;; Code:

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))


;; For consistency, we change the location of native compilation cache.
;; Currently not used
;; (when (fboundp 'startup-redirect-eln-cache)
;;   (startup-redirect-eln-cache
;;    (convert-standard-filename
;; 	  (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(defun archer/using-nix-p ()
  "Verifies if the running Emacs executable is under the `/nix/store/' path."
  (unless (or (equal system-type 'ms-dos)
              (equal system-type 'windows-nt))
    ;; Since there is no windows implementation of guix
    (string-prefix-p "/nix/store/"
                     (file-truename
                      (executable-find
                       (car command-line-args))))))

(defvar archer/config-path
  (if (archer/using-nix-p)
      (if (file-exists-p (expand-file-name ".dotfiles/config/emacs/" (getenv "HOME")))
          (expand-file-name ".dotfiles/config/emacs/" (getenv "HOME")))
    (expand-file-name user-emacs-directory)))

;; Initialise installed packages
(setq package-enable-at-startup t)

;; Allow loading from the package cache
(defvar package-quickstart)
(setq package-quickstart t)

(menu-bar-mode -1)          ; Disable the menu bar
(tool-bar-mode -1)          ; Disable the toolbar
(scroll-bar-mode -1)        ; Disable visible scrollbar
(set-fringe-mode 10)        ; Give some breathing room

(setq-default cursor-type 'bar)      ; Cursor type default
(setq inhibit-splash-screen t)       ; Inhibit horrible startup screen
(setq use-dialog-box t)              ; Mouse events dialog (yes or no predicate)
(setq use-file-dialog nil)           ; Disable dialog for files

(column-number-mode)                    ; Display column number on modeline
;;; early-init.el ends here
