;;; init-buffers.el --- Buffer navigation -*- lexical-binding: t -*-

;;; Commentary:

;; Buffer navigation and management

;;; Code:

(defun archer--human-readable-file-sizes-to-bytes (string)
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

(defun archer--bytes-to-human-readable-file-sizes (bytes)
  "Convert number of BYTES to human-readable file size."
  (interactive)
  (cond
   ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
   ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
   ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
   ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
   ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
   (t (format "%10d" bytes))))

;;; Unique names for buffers
(leaf uniquify
  :require t
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

(leaf ibuffer
  :require t
  :init
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t :summarizer
	         (lambda (column-strings)
	           (let ((total 0))
	             (dolist (string column-strings)
	               (setq total (+ (float (archer--human-readable-file-sizes-to-bytes string))
			                          total)))
	             (archer--bytes-to-human-readable-file-sizes total))))
    (archer--bytes-to-human-readable-file-sizes (buffer-size)))
  :setq
  ;; Modify the default ibuffer-formats
  (ibuffer-formats .
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
  (ibuffer-saved-filter-groups .
        '(("default"
           ("dired" (mode . dired-mode))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$"))))))
  (ibuffer-expert . t)
  (ibuffer-display-summary . t)
  (ibuffer-show-empty-filter-groups . nil)
  (ibuffer-use-other-window . nil)
  (ibuffer-movement-cycle . t)
  (ibuffer-default-sorting-mode . 'filename/process)
  (ibuffer-use-header-line . t)
  (ibuffer-default-shrink-to-minimum-size . nil)
  :hook
  (ibuffer-mode-hook . (lambda () (ibuffer-switch-to-saved-filter-groups "default")))
  :bind
  ("C-x C-b" . ibuffer))

(provide 'init-buffers)
;;; init-buffers.el ends here
