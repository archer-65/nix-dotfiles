;;; init-telega.el --- Telegram on Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; Here we go.  The idea of using Emacs for everything is (almost) real.
;; `telega' is a great client, maybe the best client around for Telegram.
;; Sometimes it has issues which depend on the version `tdlib' installed on your system, but what the hell: it's good!

;;; Code:

(setup telega
  (:pkg (:built-in 'prefer))

  (:autoload telega)

  (setopt telega-use-images t
          telega-emoji-font-family "Noto Color Emoji"
          telega-emoji-use-images nil
          telega-completing-read-function completing-read-function
          telega-animation-play-inline 2
          telega-chat-button-width 30
          switch-to-buffer-preserve-window-point t
          telega-chat--display-buffer-action '((display-buffer-reuse-window display-buffer-use-some-window))
          telega-completing-read-function 'completing-read
          telega-root-fill-column (+ 20 telega-chat-button-width))

  (:when-loaded
    (:also-load telega-mnz)
    (keymap-global-set "C-c t" 'telega-prefix-map))

  (:with-mode telega-chat-mode
    (:hook telega-mnz-mode)
    (:hook telega-completions-setup-capf))

  (:with-hook telega-load-hook
    (:hook telega-notifications-mode)))

(provide 'init-telega)
;;; init-telega.el ends here
