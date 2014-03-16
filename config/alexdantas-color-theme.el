;;; kure-color-theme.el -- My custom color theme for emacs
;; Based on the default themes by color-mode

; Emacs24's package system made me comment this
;(eval-when-compile
;  (require 'color-theme))

; Now I have a 256-colored TERMINAL, NOW!
;
; check all available colors with M-x list-colors-display
;
; check all available font customizations with:
;  1- switch to *scratch* buffer
;  2- M-x customize-face

(require 'info)

(defun alexdantas-color-theme ()
  (interactive)
  (color-theme-install
   '(alexdantas-color-theme
     ((background-color  . "default") ; or color-237
	  (background-mode	 .  dark)
	  (border-color		 . "#d4d400")
	  (cursor-color		 . "#6c6c6c")
	  (foreground-color	 . "#a8a8a8")
	  (mouse-color		 . "black"))

	 ; What is this?
	 (fringe						   ((t (:background "black"))))

	 ; Line before minibuffer
	 (mode-line						   ((t (:foreground "#8a8a8a"	:background "#262626" :bold))))
	 (mode-line-inactive			   ((t (:foreground "#8a8a8a" :background "#303030"))))
	 (mode-line-buffer-id			   ((t (:foreground "#d48000"))))
	 (mode-line-emphasis			   ((t (:foreground "#d42a00"	:bold))))
	 (mode-line-highlight			   ((t (:foreground "#550000"	:bold))))

	 ; Misc. programming stuff
	 (region						   ((t (:background "#00002a"))))
	 (hl-line						   ((t (:background "default")))) ; same as background
	 (font-lock-builtin-face		   ((t (:foreground "#2a55d4"	 :bold))))
	 (font-lock-comment-delimiter-face ((t (:foreground "#d42a00"))))
	 (font-lock-comment-face		   ((t (:foreground "#d42a00"  ))))
	 (font-lock-constant-face		   ((t (:foreground "#555500"	 :bold))))
	 (font-lock-doc-face			   ((t (:foreground "#aa002a"))))
	 (font-lock-function-name-face	   ((t (:foreground "#802a80"  :bold))))
	 (font-lock-keyword-face		   ((t (:foreground "#0080d4"	 :bold))))
	 (font-lock-negation-char-face	   ((t (:foreground "#d4aa00"	 :bold))))
	 (font-lock-preprocessor-face	   ((t (:foreground "#008000"   :bold))))
	 (font-lock-string-face			   ((t (:foreground "#008000"	 ))))
	 (font-lock-type-face			   ((t (:foreground "#0055d4"	 :bold))))
	 (font-lock-variable-name-face	   ((t (:foreground "brightwhite"))))
	 (font-lock-warning-face		   ((t (:foreground "brightred"	 :bold))))
	 (show-paren-match				   ((t (:foreground "#2ad480"	 :bold))))
	 (show-paren-mismatch			   ((t (:foreground "#552a00"	 :bold))))
	 (minibuffer-prompt				   ((t (:foreground "#00aa00"	 :bold))))

	 ; C/C++ modes
	 (c-annotation-face ((t (:inherit font-lock-constant-face :underline nil))))
	 (c-nonbreakable-space-face ((t (:background "brightred" :underline nil :weight bold))) t)
	 (completions-annotations ((t (:inherit italic :slant normal))))
	 (completions-common-part ((t (:inherit default :overline nil))))
	 (completions-first-difference ((t (:inherit bold :underline nil))))
	 (cursor ((t (:background "black" :overline nil))))

	 ; Interactive search
	 (isearch						   ((t (:background "#d4002a"	 :foreground "#eeeeee"))))
	 (isearch-fail					   ((t (:background "#2a0000"	 :foreground "#eeeeee" :bold))))
	 (lazy-highlight				   ((t (:background "#8055d4"	 :foreground "#080808"))))

	 ; Asciidoc mode
	 (markup-complex-replacement-face ((t (:foreground "#80aa00"))))
	 (markup-emphasis-face			  ((t (:foreground "cyan"))))
	 (markup-comment-face			  ((t (:foreground "#802a80"	 :bold))))
	 (markup-gen-face				  ((t (:foreground "#0000d4"))))
	 (markup-reference-face			  ((t (:foreground "#00aad4"))))
	 (markup-internal-reference-face  ((t (:foreground "#002ad4"  :bold))))
	 (markup-list-face				  ((t (:foreground "yellow"	   :bold))))
	 (markup-strong-face			  ((t (:foreground "#2ad400"  :bold))))
	 (markup-typewriter-face		  ((t (:foreground "#aa2a00"))))
	 (markup-verbatim-face			  ((t (:foreground "#d42a00" :bold))))
	 (markup-table-face			      ((t (:foreground "#800000"))))
	 (markup-meta-face			      ((t (:foreground "#800000" :bold))))
	 (markup-code-face			      ((t (:foreground "#d42a00"))))
	 (markup-meta-hide-face		      ((t (:foreground "#626262"))))
;	 (markup-gen-face	    	      ((t (:foreground "red" :bold))))
;WHY DOES THIS NOT OWRK
	 (markup-title-0-face			  ((t (:foreground "#d455d4"  :bold))))
	 (markup-title-1-face			  ((t (:foreground "#d455aa"  :bold))))
	 (markup-title-2-face			  ((t (:foreground "#d45580"  :bold))))
	 (markup-title-3-face			  ((t (:foreground "#d45555"  :bold))))
	 (markup-title-4-face			  ((t (:foreground "#d4552a"  :bold))))

	 ; Requires linum-mode (line count)
	 (linum							  ((t (:foreground "#6c6c6c" :background "#262626"))))

	 ; Requires window-number-mode
	 (window-number-face			   ((t (:foreground "#aa2a00"))))

	 ; Requires yasnippet
	 (yas-field-highlight-face		   ((t (:foreground "#0055d4" :bold))))

	 ; Makefile colors
	 (makefile-makepp-perl			   ((t (:background "LightBlue1"))))
	 (makefile-shell				   ((t (:height 1.0))) t)
	 (makefile-space				   ((t (:foreground "#d455d4" :background "black"))))
	 (makefile-targets				   ((t (:foreground "#802a00" :bold))))

	 )))

(provide 'alexdantas-color-theme)

