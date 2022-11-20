;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;; NOTE: This file is generated from `Emacs.org`!

;;; Code:

;; Add load-path for submodules
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; We don't want user customizations in `init.el`, instead we use `custom.el`
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Set a better directory to store the native comp cache
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (add-to-list 'native-comp-eln-load-path (expand-file-name "var/eln-cache/" user-emacs-directory)))

;; Disable damn sleep!
;; Yep, it's mandatory, that's the worst keybind ever, and should be remapped
(global-unset-key (kbd "C-z"))

(defun archer-using-nix-p ()
  "Verifies if the running Emacs executable is under the `/nix/store/' path."
  (unless (or (equal system-type 'ms-dos)
              (equal system-type 'windows-nt))
    ;; Since there is no windows implementation of nix
    (string-prefix-p "/nix/store/"
                     (file-truename
                      (executable-find
                       (car command-line-args))))))

(defvar archer-config-path
  (if (archer-using-nix-p)
      (if (file-exists-p (expand-file-name ".dotfiles/config/emacs/" (getenv "HOME")))
          (expand-file-name ".dotfiles/config/emacs/" (getenv "HOME")))
    (expand-file-name user-emacs-directory)))

;; Require package management file
(require 'init-packages)

(require 'init-performance)

(require 'init-help)

(require 'init-fonts)

(require 'init-appearance)

(require 'init-dash)

(require 'init-modeline)

(require 'init-editing)

;; (require 'init-meow)

(require 'init-windows)

(require 'init-buffers)

(require 'init-dired)

(require 'init-complete)

(require 'init-embark)

(require 'init-consult)

(require 'init-complete-in-buffer)

(require 'init-org)

(require 'init-org-languages)

(require 'init-org-export)

(require 'init-projects)

(require 'init-code-style)

(require 'init-spell-and-check)

(require 'init-lsp)

(require 'init-snippets)

(require 'init-extra-modes)

(require 'init-mail)

(require 'init-pdf)

(require 'init-shell)

(require 'init-telega)

(require 'init-media)

(setup (:pkg daemons))

;;; init.el ends here
