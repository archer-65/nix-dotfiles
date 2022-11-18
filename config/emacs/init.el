;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;; NOTE: This file is generated from `Emacs.org`!

;;; Code:

;; Add load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; We don't want customizations in `init.el`, instead we use `custom.el`.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Set the right directory to store the native comp cache
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (add-to-list 'native-comp-eln-load-path (expand-file-name "var/eln-cache/" user-emacs-directory)))

;; Disable damn sleep!
;; Yep, it's mandatory, that's the worst keybind ever.
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

;; Require package management file.
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

(leaf daemons
  :straight t)

(leaf vterm
  :commands (vterm vterm-other-window)
  :init
  (unless (archer-using-nix-p) straight-use-package 'vterm)
  :bind
  ("<f5>" . vterm)
  :config
  (setq-default vterm-buffer-name " <<Terminal>>")
  (add-to-list 'display-buffer-alist
               '("\xe795 <<Terminal>>" ;; Original regex: "\*vterm\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.25)
                 (side . bottom)
                 (slot . 0))))

(leaf telega
  :commands (telega)
  :init
  (unless (archer-using-nix-p) straight-use-package 'telega)
  (setq telega-directory (expand-file-name "~/.telega/"))
  (setq telega-server-libs-prefix (expand-file-name "~/.nix-profile"))
  (setq telega-use-images t)
  (setq telega-emoji-font-family "Noto Color Emoji")
  (setq telega-emoji-use-images nil)
  :config
  (setq telega-emoji-company-backend 'telega-company-emoji)

  ;; From Andrew Tropin <3
  (defun archer-telega-chat-mode ()
    "Add completion at point functions made from company backends."
    (setq-local
     completion-at-point-functions
     (append (mapcar
              'cape-company-to-capf
              (append (list 'telega-company-emoji
                            'telega-company-username
                            'telega-company-hashtag)
                      (when (telega-chat-bot-p telega-chatbuf--chat)
                        '(telega-company-botcmd))))
             completion-at-point-functions)))
  (add-hook 'telega-chat-mode-hook 'archer-telega-chat-mode)

  (setq telega-completing-read-function completing-read-function)

  (require 'telega-mnz)
  (setq telega-animation-play-inline 2)
  (setq telega-inserter-for-chat-button 'telega-ins--chat-full-2lines)
  (setq telega-chat-button-width 30)
  (setq telega-root-fill-column (+ 20 telega-chat-button-width))
  (put (get 'telega-chat 'button-category-symbol)
       :inserter 'telega-ins--chat-full-2lines)
  (setq switch-to-buffer-preserve-window-point t)
  (setq telega-chat--display-buffer-action
        '((display-buffer-reuse-window display-buffer-use-some-window)))
  (define-key global-map (kbd "C-c t") telega-prefix-map)
  (setq telega-completing-read-function 'completing-read)
  :hook
  (telega-load-hook . telega-notifications-mode)
  (telega-chat-mode-hook . telega-mnz-mode))

(leaf emms
  :straight t
  :init
  ;; Notification on play
  (defun emms-notify-track-description ()
    "Use `notify-send' to show the description of the currecnt track."
    (call-process
     "notify-send"
     nil nil nil
     "-a" "EMMS"
     "-t" "1000"
     "-h" "string:x-dunst-stack-tag:test"
     "-a" "music"
     (emms-track-description
      (emms-playlist-current-selected-track))))
  :config
  ;; Start
  (require 'emms-setup)
  (require 'emms-mode-line)
  (require 'emms-playing-time)
  (emms-all)

  ;; Info
  (setq emms-mode-line t)
  (setq emms-playing-time t)

  ;; Directory
  ;; (setq emms-source-file-default-directory "~/idkrn/")
  (setq emms-info-asynchronously t)

  ;; Other infos, covers
  (setq emms-info-functions '(emms-info-exiftool)
        emms-browser-covers 'emms-browser-cache-thumbnail-async)
  :hook
  (emms-player-started-hook . emms-notify-track-description))

;;; init.el ends here
