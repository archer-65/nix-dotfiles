;;; early-init.el --- Early Init Fileit loads basic stuff before GUI is initialized

;;; Commentary:

;; Starting with Emacs 27, the default
;; behaviour is to start the package manager before loading the init
;; file.

;;; Code:

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 128 1024 1024))

(defun archer-65/display-startup-time ()
  "This function serves as convenience to display startup time after Emacs start."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

;; Startup hook to display startup time.
(add-hook 'emacs-startup-hook #'archer-65/display-startup-time)

;; Initialise installed packages
(setq package-enable-at-startup nil)

;; Allow loading from the package cache
(defvar package-quickstart)
(setq package-quickstart t)

(menu-bar-mode -1)          ; Disable the menu bar
(tool-bar-mode -1)          ; Disable the toolbar
(scroll-bar-mode -1)        ; Disable visible scrollbar
;; (tooltip-mode -1)        ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(setq-default cursor-type 'bar)      ; Cursor type default
(setq inhibit-splash-screen t)       ; Inhibit horrible startup screen
(setq use-dialog-box t)              ; Mouse events dialog (yes or no predicate)
(setq use-file-dialog nil)           ; Disable dialog for files

(column-number-mode)                    ; Display column number on modeline
(global-display-line-numbers-mode t)    ; Enable numbers globally

;;; early-init.el ends here
