;;; init-themes.el --- Themes -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration of `modus-themes' and `ef-themes', high accessibility themes by Protesilaos.

;;; Code:


(setup modus-themes
  (:elpaca t)
  ;; Preferences
  (:option modus-themes-org-blocks 'gray-background
           modus-themes-mixed-fonts nil
           modus-themes-variable-pitch-ui nil)
  
  ;; Overrides
  (:option modus-themes-common-palette-overrides
           ;; Modeline
           '((bg-mode-line-active bg-blue-subtle)
             (fg-mode-line-active fg-main)
             (border-mode-line-active blue-intense)
             ;; Region
             (bg-region bg-lavender)
             (fg-region unspecified)
             ;; Mouse Hovers
             (bg-hover bg-yellow-intense)
             ;; Fringe
             (fringe unspecified)
             ;; Inline code in prose (markup)
             (prose-block fg-dim)
             (prose-code green-cooler)
             (prose-done green)
             (prose-macro magenta-cooler)
             (prose-metadata fg-dim)
             (prose-metadata-value fg-alt)
             (prose-table fg-alt)
             (prose-tag magenta-faint)
             (prose-todo red)
             (prose-verbatim magenta-warmer)
             ;; Syntax
             (comment yellow-faint)
             (string green-warmer)
             ;; Checkers
             (underline-err red-faint)
             (underline-warning yellow-faint)
             (underline-note cyan-faint)
             ;; Links - No underlines
             (underline-link unspecified)
             (underline-link-visited unspecified)
             (underline-link-symbolic unspecified)
             ;; Box buttons
             (bg-button-active bg-main)
             (fg-button-active fg-main)
             (bg-button-inactive bg-inactive)
             (fg-button-inactive "gray50")
             ;; Prompts
             (fg-prompt cyan)
             (bg-prompt bg-cyan-nuanced)
             ;; Completion
             (fg-completion-match-0 fg-main)
             (fg-completion-match-1 fg-main)
             (fg-completion-match-2 fg-main)
             (fg-completion-match-3 fg-main)
             (bg-completion-match-0 bg-blue-subtle)
             (bg-completion-match-1 bg-yellow-subtle)
             (bg-completion-match-2 bg-cyan-subtle)
             (bg-completion-match-3 bg-red-subtle)
             ;; Mail citations
             (mail-cite-0 blue)
             (mail-cite-1 yellow)
             (mail-cite-2 green)
             (mail-cite-3 magenta)
             (mail-part magenta-cooler)
             (mail-recipient cyan)
             (mail-subject red-warmer)
             (mail-other cyan-cooler)
             ;; Line numbers
             (fg-line-number-inactive "gray50")
             (fg-line-number-active fg-main)
             (bg-line-number-inactive unspecified)
             (bg-line-number-active unspecified)))

  (modus-themes-select 'modus-operandi))

(setup ef-themes
  (:elpaca t))

;; I set circadian in the configuration of my themes
(setup circadian
  (:elpaca t)
  (:load-after modus-themes)
  (:option circadian-themes '(("8:00" . modus-operandi)
                              ("20:00" . modus-vivendi)))
  (circadian-setup))

(provide 'init-themes)
;;; init-themes.el ends here
