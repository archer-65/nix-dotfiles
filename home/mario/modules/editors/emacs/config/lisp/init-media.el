;;; init-media.el --- Manage your medias and more from Emacs O-O -*- lexical-binding: t -*-

;;; Commentary:

;; This section is poor right now, but should contain multimedia functionality to avoid leaving Emacs.

;;; Code:

(setup (:pkg mpv))

(setup (:pkg emms)
  (:require emms-setup)
  (emms-all)

  (:option emms-mode-line t
           ;; emms-source-file-default-directory "~/idkrn/"
           emms-info-asynchronously t
           emms-playing-time t
           emms-info-functions '(emms-info-exiftool)
           emms-browser-covers 'emms-browser-cache-thumbnail-async)

  (add-hook 'emms-player-started-hook #'emms-notify-track-description))

(provide 'init-media)
;;; init-media.el ends here
