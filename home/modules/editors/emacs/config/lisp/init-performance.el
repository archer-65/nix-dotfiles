;;; init-performance.el --- Performances enhancement -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain tweaks to obtain better overall performances.

;;; Code:


(setup (:pkg gcmh)
  (:require)
  (:hide-mode)
  ;; The GC introduces annoying pauses and stuttering into our Emacs experience,
  ;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
  ;; when it's idle. However, if the idle delay is too long, we run the risk of
  ;; runaway memory usage in busy sessions. If it's too low, then we may as well
  ;; not be using gcmh at all.
  (:option gcmh-idle-delay 'auto ; Default 15 seconds
           gcmh-auto-idle-delay-factor 10
           gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-mode 1))

;; Aaand, here other code stolen from DOOM.
;; Performances are really better with this snippet (for me).
(setup tweaks
  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
  ;; is more than enough.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-default-init t
        ;; Shave seconds off startup time by starting the scratch buffer in
        ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
        ;; pull in a ton of packages.
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil)

  ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
  (setq idle-update-delay 1.0)
  ;; Resizing the Emacs frame can be a terribly expensive part of changing the
  ;; font. By inhibiting this, we halve startup times, particularly when we use
  ;; fonts that are larger than the system default (which would resize the frame).
  (setq frame-inhibit-implied-resize t)

  ;; PGTK builds only: this timeout adds latency to frame operations, like
  ;; `make-frame-invisible', which are frequently called without a guard because
  ;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
  ;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
  ;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
  (setq pgtk-wait-for-event-timeout 0.001)

  ;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
  ;; receiving input, which should help a little with scrolling performance.
  (setq redisplay-skip-fontification-on-input t))

(provide 'init-performance)
;;; init-appearance.el ends here
