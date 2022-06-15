;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:

;; Starting with Emacs 27, the default behaviour is
;; to start the package manager before loading the init file.
;; This file loads basic stuff before GUI is initialized.

;;; Code:

(defvar default-gc-cons-threshold (* 2 1000 1000) ;; 16MB
  "Desired value of `gc-cons-threshold' during normal Emacs operations.")

;; Make garbage collector less invasive
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.6)

;; From DOOM
(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

;; Message, stupid message.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

;; Reset defaults for garbage collector and `default-file-name-handler-alist`
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold default-gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)
            ;; delete no longer necessary startup variable
            (makunbound 'default-file-name-handler-alist)))

;; Initialise installed packages
(setq package-enable-at-startup t)

;; Allow loading from the package cache
(defvar package-quickstart)
(setq package-quickstart t)

;; If native compilation is enabled, disable warnings
(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil))

;; Minor tweaks for graphics
(setq-default cursor-type 'bar)      ; Cursor type default
(setq inhibit-splash-screen t)       ; Inhibit horrible startup screen
(setq use-dialog-box t)              ; Mouse events dialog (yes or no predicate)
(setq use-file-dialog nil)           ; Disable dialog for files

(menu-bar-mode -1)          ; Disable the menu bar
(tool-bar-mode -1)          ; Disable the toolbar
(scroll-bar-mode -1)        ; Disable visible scrollbar
(set-fringe-mode 10)        ; Give some breathing room
(column-number-mode)        ; Display column number on mode-line

;; Disable redisplay until emacs is loaded
;; (setq-default inhibit-redisplay t)
;; (add-hook 'window-setup-hook
;;          (lambda ()
;;            (setq-default inhibit-redisplay nil)
;;            (redisplay)))

;;; early-init.el ends here
