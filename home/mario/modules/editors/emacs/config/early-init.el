;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:

;; Early init file has been introduced in Emacs 27, it is a file loaded
;; before GUI is initialized, so unwanted elements are here.
;; Example: scroll-bars, fringes, menu-bar, tool-bar.

;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'.
(setq gc-cons-threshold  most-positive-fixnum)

;; Add load-path for submodules
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set a better directory to store the native comp cache
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (add-to-list 'native-comp-eln-load-path (expand-file-name "var/eln-cache/" user-emacs-directory)))

;; From DOOM
;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(when (and (fboundp 'native-comp-available-p)
     (native-comp-available-p))
  (setq native-comp-deferred-compilation nil)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil))

;; Another trick from DOOM
(unless (or (daemonp)
            noninteractive
            init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)
    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (defun doom-reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h 101))

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay))))

;; From DOOM
;;
;; NOTE: In DOOM these are defined in another file, not in early init, that's horrible because
;; starting a client where this settings are defined later causes a little flash at startup (before redisplay)
;; where menu-bar is present.
;;
;; Not calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because they do extra and unnecessary work that can be more
;; concisely and efficiently expressed with these six lines:
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      column-number-mode t
      fringe-mode 10)

;; Minor message for gc after loading
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil
      package-quickstart nil)

;; Configure and bootstrap `straight.el'
(setq straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-profiles `((nil . ,(expand-file-name "straight/versions/lock.el" user-emacs-directory))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Additional post-setup of `straight.el'
(require 'straight-x)
(defalias 'straight-ಠ_ಠ-mode nil)

;;; early-init.el ends here
