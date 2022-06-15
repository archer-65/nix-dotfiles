;;; init-telega.el --- Embark, run a command based on point-*- lexical-binding: t -*-

;;; Commentary:

;; Sometimes you want to act near point, but there are many actions.
;; Embark ships many actions, dependant on target and modes.

;;; Code:

(leaf telega
  :straight t
  :commands telega)

(leaf rainbow-identifiers
  :straight t
  :after telega)

(provide 'init-telega)
;;; init-telega.el ends here
