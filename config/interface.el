;; Appearance settings

; Color theme mode: my theme's already on the load path
; Enabling the color-theme mode and selecting my own
(require 'color-theme)
(load "~/.emacs.d/config/alexdantas-color-theme.el")
(alexdantas-color-theme)

; Zenburn is an awesome theme, I must someday get colors from
; this and implement on mine
;(load-theme 'zenburn t)

; Dired mode: switches passed to 'ls' when viewing dired mode
(setq dired-listing-switches "-lahpD --color=auto")

; Make default way of splitting windows horizontally (side-by-side)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

