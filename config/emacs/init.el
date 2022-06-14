;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Still here, just for compatibility purpose.
(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el")))

;; Add load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; We don't want customizations in `init.el`, instead we use `custom.el`.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Set the right directory to store the native comp cache
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Disable damn sleep!
;; Yep, it's mandatory, that's the worst keybind ever.
(global-unset-key (kbd "C-z"))

(defun archer/using-nix-p ()
  "Verifies if the running Emacs executable is under the `/nix/store/' path."
  (unless (or (equal system-type 'ms-dos)
              (equal system-type 'windows-nt))
    ;; Since there is no windows implementation of nix
    (string-prefix-p "/nix/store/"
                     (file-truename
                      (executable-find
                       (car command-line-args))))))

(defvar archer/config-path
  (if (archer/using-nix-p)
      (if (file-exists-p (expand-file-name ".dotfiles/config/emacs/" (getenv "HOME")))
          (expand-file-name ".dotfiles/config/emacs/" (getenv "HOME")))
    (expand-file-name user-emacs-directory)))

;; Require package management file.
(require 'init-packages)

;; The `no-littering` package to keep folders where we edit files and the Emacs configuration folder clean.
(leaf no-littering
  :doc "Keeps folders clean"
  :ensure t
  :setq
  ;; The package `no-littering` doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (auto-save-file-name-transforms . `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(leaf gcmh
  :ensure t
  :require t
  :config
  (gcmh-mode 1))

(require 'init-help)

(require 'init-appearance)

(require 'init-fonts)

(require 'init-modeline)

(require 'init-dash)

(require 'init-complete)

(require 'init-consult)

(require 'init-embark)

(require 'init-complete-in-buffer)

(require 'init-editing)

;; (require 'init-meow)

(require 'init-windows)

(require 'init-buffers)

(require 'init-dired)

(require 'init-org)

(require 'init-org-languages)

(require 'init-org-export)

(require 'init-projects)

(require 'init-code-style)

(require 'init-spell-and-check)

(require 'init-extra-modes)

(require 'init-snippets)

(require 'init-lsp)

(require 'init-mail)

(require 'init-pdf)

;; Make gc pauses faster by decreasing the threshold.
;; (setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
