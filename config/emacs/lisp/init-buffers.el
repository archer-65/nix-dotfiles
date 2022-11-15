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

(leaf ibuffer
  :require t
  :config
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
  (setq ibuffer-formats
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
  (setq ibuffer-saved-filter-groups
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
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary t)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-movement-cycle t)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  :hook
  (ibuffer-mode-hook . (lambda ()
                         (ibuffer-switch-to-saved-filter-groups "default")
                         (ibuffer-auto-mode 1)))
  :bind
  ("C-x C-b" . ibuffer))

;;; Unique names for buffers
(leaf uniquify
  :require t
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

(provide 'init-buffers)
;;; init-buffers.el ends here
