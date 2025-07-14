;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;; NOTE: This file is generated from `Emacs.org`!

;;; Code:

;; We don't want user customizations in `init.el`, instead we use `custom.el`
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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
  (let ((real-path (expand-file-name
                    ".dotfiles/home/modules/editors/emacs/config/"
                    (getenv "HOME"))))
    (if (and (archer-using-nix-p)
             (file-exists-p real-path))
        (expand-file-name real-path)
      (expand-file-name user-emacs-directory))))

;; Require package management file
(require 'init-setup)

(setup exec-path-from-shell
  (:pkg t)
  (:only-if (eq system-type 'darwin))
  (:require exec-path-from-shell)
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize)

  ;; Put this here because I installed coreutils with Nix
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq dired-use-ls-dired t
              insert-directory-program gls)))))
t
(when (eq system-type 'darwin)
  ;; mac-* variables are used by the special emacs-mac build of Emacs by
  ;; Yamamoto Mitsuharu, while other builds use ns-*.
  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none))

(require 'init-performance)

(require 'init-help)

(require 'init-fonts)

(require 'init-themes)

(require 'init-appearance)

(require 'init-modeline)

(require 'init-dash)

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

(require 'init-tex)

(require 'init-pdf)

(setup beancount-mode
  (:pkg (:host github :repo "beancount/beancount-mode" :main "beancount.el")))

(require 'init-shell)

(require 'init-telega)

(require 'init-media)

(setup daemons
  (:pkg t))

;;; init.el ends here
