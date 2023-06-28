;;; init-appearance.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain appearance settings stuff.

;;; Code:

(setup appearance
  ;; A simple frame title
  (setq frame-title-format '("%b – Emacs")
        icon-title-format frame-title-format)

  ;; Stuff
  (setq calendar-date-style 'european)
  (setq display-time-default-load-average nil)
  (setq highlight-nonselected-windows nil)
  (setq echo-keystrokes 0.1)

  ;; Other graphical stuff
  (setq visible-bell nil)
  (setq x-gtk-use-system-tooltips t)
  (setq x-stretch-cursor nil)

  ;; Dialogs
  (setq use-dialog-box nil      ; Mouse events dialog
        use-file-dialog nil)    ; Disable dialog for files

  ;; Cursor
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default cursor-type 'bar)
  (blink-cursor-mode 0)

  ;; Bidirectional settings
  (setq-default bidi-display-reordering 'left-to-right)
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Lines related
  (setq-default truncate-lines nil)
  (setq-default visual-line-mode t)

  (setq-default indicate-buffer-boundaries nil))

;; You must run `all-the-icons-install-fonts` the first time.
(setup (:pkg all-the-icons)
  (:require all-the-icons))

(provide 'init-appearance)
;;; init-appearance.el ends here
