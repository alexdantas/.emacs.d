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
	  (border-color		 . "color-226")
	  (cursor-color		 . "color-242")
	  (foreground-color	 . "color-248")
	  (mouse-color		 . "black"))

	 ; What is this?
	 (fringe						   ((t (:background "black"))))

	 ; Line before minibuffer
	 (mode-line						   ((t (:foreground "color-245"	:background "color-235" :bold))))
	 (mode-line-inactive			   ((t (:foreground "color-245" :background "color-236"))))
	 (mode-line-buffer-id			   ((t (:foreground "color-214"))))
	 (mode-line-emphasis			   ((t (:foreground "color-202"	:bold))))
	 (mode-line-highlight			   ((t (:foreground "color-88"	:bold))))

	 ; Misc. programming stuff
	 (region						   ((t (:background "color-17"))))
	 (hl-line						   ((t (:background "default")))) ; same as background
	 (font-lock-builtin-face		   ((t (:foreground "color-69"	 :bold))))
	 (font-lock-comment-delimiter-face ((t (:foreground "color-202"))))
	 (font-lock-comment-face		   ((t (:foreground "color-202"  ))))
	 (font-lock-constant-face		   ((t (:foreground "color-100"	 :bold))))
	 (font-lock-doc-face			   ((t (:foreground "color-161"))))
	 (font-lock-function-name-face	   ((t (:foreground "color-133"  :bold))))
	 (font-lock-keyword-face		   ((t (:foreground "color-39"	 :bold))))
	 (font-lock-negation-char-face	   ((t (:foreground "color-220"	 :bold))))
	 (font-lock-preprocessor-face	   ((t (:foreground "color-34"   :bold))))
	 (font-lock-string-face			   ((t (:foreground "color-34"	 ))))
	 (font-lock-type-face			   ((t (:foreground "color-33"	 :bold))))
	 (font-lock-variable-name-face	   ((t (:foreground "brightwhite"))))
	 (font-lock-warning-face		   ((t (:foreground "brightred"	 :bold))))
	 (show-paren-match				   ((t (:foreground "color-85"	 :bold))))
	 (show-paren-mismatch			   ((t (:foreground "color-94"	 :bold))))
	 (minibuffer-prompt				   ((t (:foreground "color-40"	 :bold))))

	 ; C/C++ modes
	 (c-annotation-face ((t (:inherit font-lock-constant-face :underline nil))))
	 (c-nonbreakable-space-face ((t (:background "brightred" :underline nil :weight bold))) t)
	 (completions-annotations ((t (:inherit italic :slant normal))))
	 (completions-common-part ((t (:inherit default :overline nil))))
	 (completions-first-difference ((t (:inherit bold :underline nil))))
	 (cursor ((t (:background "black" :overline nil))))

	 ; Interactive search
	 (isearch						   ((t (:background "color-197"	 :foreground "color-255"))))
	 (isearch-fail					   ((t (:background "color-52"	 :foreground "color-255" :bold))))
	 (lazy-highlight				   ((t (:background "color-141"	 :foreground "color-232"))))

	 ; Asciidoc mode
	 (markup-complex-replacement-face ((t (:foreground "color-148"))))
	 (markup-emphasis-face			  ((t (:foreground "cyan"))))
	 (markup-comment-face			  ((t (:foreground "color-133"	 :bold))))
	 (markup-gen-face				  ((t (:foreground "color-21"))))
	 (markup-reference-face			  ((t (:foreground "color-45"))))
	 (markup-internal-reference-face  ((t (:foreground "color-27"  :bold))))
	 (markup-list-face				  ((t (:foreground "yellow"	   :bold))))
	 (markup-strong-face			  ((t (:foreground "color-82"  :bold))))
	 (markup-typewriter-face		  ((t (:foreground "color-166"))))
	 (markup-verbatim-face			  ((t (:foreground "color-202" :bold))))
	 (markup-table-face			      ((t (:foreground "color-124"))))
	 (markup-meta-face			      ((t (:foreground "color-124" :bold))))
	 (markup-code-face			      ((t (:foreground "color-202"))))
	 (markup-meta-hide-face		      ((t (:foreground "color-241"))))
;	 (markup-gen-face	    	      ((t (:foreground "red" :bold))))
;WHY DOES THIS NOT OWRK
	 (markup-title-0-face			  ((t (:foreground "color-213"  :bold))))
	 (markup-title-1-face			  ((t (:foreground "color-212"  :bold))))
	 (markup-title-2-face			  ((t (:foreground "color-211"  :bold))))
	 (markup-title-3-face			  ((t (:foreground "color-210"  :bold))))
	 (markup-title-4-face			  ((t (:foreground "color-209"  :bold))))

	 ; Requires linum-mode (line count)
	 (linum							  ((t (:foreground "color-242" :background "color-235"))))

	 ; Requires window-number-mode
	 (window-number-face			   ((t (:foreground "color-166"))))

	 ; Requires yasnippet
	 (yas-field-highlight-face		   ((t (:foreground "color-33" :bold))))

	 ; Makefile colors
	 (makefile-makepp-perl			   ((t (:background "LightBlue1"))))
	 (makefile-shell				   ((t (:height 1.0))) t)
	 (makefile-space				   ((t (:foreground "color-213" :background "black"))))
	 (makefile-targets				   ((t (:foreground "color-130" :bold))))

	 )))

(provide 'alexdantas-color-theme)

