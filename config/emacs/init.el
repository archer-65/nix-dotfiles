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
  :straight t
  :setq
  ;; The package `no-littering` doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (auto-save-file-name-transforms . `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(leaf gcmh
  :straight t
  :init
  ;; The GC introduces annoying pauses and stuttering into our Emacs experience,
  ;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
  ;; when it's idle. However, if the idle delay is too long, we run the risk of
  ;; runaway memory usage in busy sessions. If it's too low, then we may as well
  ;; not be using gcmh at all.
  (setq gcmh-idle-delay 'auto ; Default 15 seconds
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 1024)) ; 16MB
  :require t
  :config
  (gcmh-mode 1))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; PGTK builds only: this timeout adds latency to frame operations, like
;; `make-frame-invisible', which are frequently called without a guard because
;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
(setq pgtk-wait-for-event-timeout 0.001)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages. `doom/open-scratch-buffer' provides a better
      ;; scratch buffer anyway.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Introduced in Emacs 28
(setq use-short-answers t)

(require 'init-help)

(require 'init-fonts)

(require 'init-appearance)

(require 'init-dash)

(require 'init-modeline)

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

(leaf pomodoro
  :straight t
  :require t
  :custom
  (pomodoro-desktop-notification . t)
  :config
  (pomodoro-add-to-mode-line))

(require 'init-pdf)

(leaf daemons
  :straight t)

(leaf vterm
  :commands (vterm vterm-other-window)
  :init
  (unless (archer/using-nix-p) straight-use-package 'vterm)
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
  (unless (archer/using-nix-p) straight-use-package 'telega)
  (setq telega-directory (expand-file-name "~/.telega/"))
  (setq telega-server-libs-prefix (expand-file-name "~/.nix-profile"))
  (setq telega-use-images t)
  (setq telega-emoji-font-family "Noto Color Emoji")
  (setq telega-emoji-use-images nil)
  :config
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
