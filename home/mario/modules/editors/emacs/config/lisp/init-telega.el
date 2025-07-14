;;; init-telega.el --- Telegram on Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; Here we go.  The idea of using Emacs for everything is (almost) real.
;; `telega' is a great client, maybe the best client around for Telegram.
;; Sometimes it has issues which depend on the version `tdlib' installed on your system, but what the hell: it's good!

;;; Code:

(setup telega
  (:pkg (:nix t))

  (:autoload telega)

  (:option telega-use-images t
           telega-emoji-font-family "Noto Color Emoji"
           telega-emoji-use-images nil
           telega-emoji-company-backend 'telega-company-emoji
           telega-completing-read-function completing-read-function
           telega-animation-play-inline 2
           telega-inserter-for-chat-button 'telega-ins--chat-full-2lines
           telega-chat-button-width 30
           switch-to-buffer-preserve-window-point t
           telega-chat--display-buffer-action '((display-buffer-reuse-window display-buffer-use-some-window))
           telega-completing-read-function 'completing-read
           telega-root-fill-column (+ 20 telega-chat-button-width))

  (put (get 'telega-chat 'button-category-symbol)
       :inserter 'telega-ins--chat-full-2lines)

  (:when-loaded
    (:also-load telega-mnz)
    (:global "C-c t" telega-prefix-map))

  (:with-mode telega-chat-mode
    ;; (:hook archer-telega-chat-mode)
    ;; From Andrew Tropin <3
    (:local-set completion-at-point-functions (mapcar
                                               #'cape-company-to-capf
                                               (append (list 'telega-company-emoji
                                                             'telega-company-username
                                                             'telega-company-hashtag)
                                                       (when (telega-chat-bot-p telega-chatbuf--chat)
                                                         '(telega-company-botcmd)))))
    (:hook telega-mnz-mode))

  (:with-hook telega-load-hook
    (:hook telega-notifications-mode)))

(provide 'init-telega)
;;; init-telega.el ends here
