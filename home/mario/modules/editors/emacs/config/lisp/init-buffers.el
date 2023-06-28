;;; init-buffers.el --- Buffer navigation -*- lexical-binding: t -*-

;;; Commentary:

;; Buffer navigation and management

;;; Code:

(defun archer-human-readable-file-sizes-to-bytes (string)
  "Convert a human-readable file (as STRING) size into BYTES."
  (interactive)
  (cond
   ((string-suffix-p "G" string t)
    (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "M" string t)
    (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "K" string t)
    (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
   (t
    (string-to-number (substring string 0 (- (length string) 1))))))

(defun archer-bytes-to-human-readable-file-sizes (bytes)
  "Convert number of BYTES to human-readable file size."
  (interactive)
  (cond
   ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
   ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
   ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
   ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
   ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
   (t (format "%10d" bytes))))

(setup (:require ibuffer)
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t :summarizer
           (lambda (column-strings)
             (let ((total 0))
               (dolist (string column-strings)
                 (setq total (+ (float (archer-human-readable-file-sizes-to-bytes string))
                                total)))
               (archer-bytes-to-human-readable-file-sizes total))))
    (archer-bytes-to-human-readable-file-sizes (buffer-size)))
  ;; Modify the default ibuffer-formats
  (:option ibuffer-formats
           '((mark modified read-only locked " "
                   (name 20 20 :left :elide)
                   " "
                   (size-h 11 -1 :right)
                   " "
                   (mode 16 16 :left :elide)
                   " "
                   filename-and-process)
             (mark " "
                   (name 16 -1)
                   " " filename)))
  ;; Add groups
  (:option ibuffer-saved-filter-groups
           '(("default"
              ("dired" (mode . dired-mode))
              ("git"   (or (mode . magit-mode)
                           (mode . magit-process-mode)
                           (mode . magit-diff-mode)
                           (mode . magit-status-mode)))
              ("elisp" (mode . emacs-lisp-mode))
              ("c"     (mode . c-mode))
              ("c++" (mode . c++-mode))
              ("nix" (mode . nix-mode))
              ("rust" (mode . rustic-mode))
              ("java" (mode . java-mode))
              ("telegram"  (or (mode . telega-root-mode)
                               (mode . telega-mode)
                               (mode . telega-chat-mode)))
              ("documents" (or (name . "\\.pdf")
                               (name . "\\.org")))
              ("mails" (or (mode . notmuch-show-mode)
                           (mode . notmuch-tree-mode)
                           (mode . notmuch-search-mode)
                           (mode . notmuch-message-mode)))
              ("emacs" (or
                        (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Warnings\\*$")
                        (name . "^\\*straight-process\\*$")
                        (name . "^\\*dashboard\\*$"))))))

  (:option ibuffer-expert t
           ibuffer-display-summary t
           ibuffer-show-empty-filter-groups nil
           ibuffer-use-other-window nil
           ibuffer-movement-cycle t
           ibuffer-default-sorting-mode 'filename/process
           ibuffer-use-header-line t
           ibuffer-default-shrink-to-minimum-size nil)

  (:hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")
           (ibuffer-auto-mode 1)))

  (:global "C-x C-b" ibuffer))

;;; Unique names for buffers
(setup (:require uniquify)
  (:option uniquify-buffer-name-style 'forward
           uniquify-strip-common-suffix t
           uniquify-after-kill-buffer-p t))

(setup desktop
  (setq desktop-auto-save-timeout 300
           desktop-path `(,user-emacs-directory)
           desktop-base-file-name "desktop"
           desktop-files-not-to-save nil
           desktop-buffers-not-to-save nil
           desktop-globals-to-clear nil
           desktop-load-locked-desktop t
           desktop-missing-file-warning nil
           desktop-restore-eager 0
           desktop-restore-frames nil
           desktop-save 'ask-if-new)

  (:when-loaded
    (dolist (symbol '(kill-ring file-name-history))
      (add-to-list 'desktop-globals-to-save symbol)))

  (desktop-save-mode 1))

(provide 'init-buffers)
;;; init-buffers.el ends here
