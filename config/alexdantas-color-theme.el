;; alexdantas-color-theme.el -- My custom color theme for emacs
;; Based on the default themes by color-mode
;;
;; Check all available colors with M-x list-colors-display
;;
;; Check all available font customizations with:
;;  1- switch to *scratch* buffer
;;  2- M-x customize-face

(require 'info)

(defun alexdantas-color-theme ()
  (interactive)
  (color-theme-install
   '(alexdantas-color-theme
	 ((background-color  . "black")
	  (background-mode	 . dark)
	  (border-color		 . "black")
	  (cursor-color		 . "white")
	  (foreground-color	 . "white")
	  (mouse-color		 . "black"))

	 ; What is this?
	 (fringe ((t (:background "black"))))

	 ; Line before minibuffer
	 (mode-line			  ((t (:foreground "#8a8a8a"	 :bold))))
	 (mode-line-inactive  ((t (:foreground "#585858"))))
	 (mode-line-buffer-id ((t (:foreground "#ffaf00"))))
	 (mode-line-emphasis  ((t (:foreground "#ff5f00"	 :bold))))
	 (mode-line-highlight ((t (:foreground "#870000"	 :bold))))

	 ; Misc. programming stuff
	 (region						   ((t (:foreground "#ffffff" :background "#898941"))))
	 (hl-line						   ((t (:background "black"))))
	 (font-lock-builtin-face		   ((t (:foreground "#f0dfaf"	 :bold))))
	 (font-lock-comment-delimiter-face ((t (:foreground "#7f9f7f"))))
	 (font-lock-comment-face		   ((t (:foreground "#7f9f7f"	 :bold))))
	 (font-lock-constant-face		   ((t (:foreground "#8cd0d3"	 :bold))))
	 (font-lock-doc-face			   ((t (:foreground "#cc9393"))))
	 (font-lock-function-name-face	   ((t (:foreground "#efef8f"))))
	 (font-lock-keyword-face		   ((t (:foreground "#f0dfaf"	 :bold))))
	 (font-lock-negation-char-face	   ((t (:foreground "#ffd700"	 :bold))))
	 (font-lock-preprocessor-face	   ((t (:foreground "#ffcfaf"))))
	 (font-lock-string-face			   ((t (:foreground "#cc9393"	 :bold))))
	 (font-lock-type-face			   ((t (:foreground "#dfdfbf"	 :bold))))
	 (font-lock-variable-name-face	   ((t (:foreground "brightwhite"))))
	 (font-lock-warning-face		   ((t (:foreground "#ffffff"	 :background "#333333" :bold))))
	 (show-paren-match				   ((t (:foreground "#5fffaf"	 :bold))))
	 (show-paren-mismatch			   ((t (:foreground "#875f00"	 :bold))))
	 (minibuffer-prompt				   ((t (:foreground "#00d700"	 :bold))))

	 ; C/C++ modes
	 (c-annotation-face            ((t (:inherit font-lock-constant-face :underline nil))))
	 (c-nonbreakable-space-face    ((t (:background "brightred" :underline nil :weight bold))) t)
	 (completions-annotations      ((t (:inherit italic :slant normal))))
	 (completions-common-part      ((t (:inherit default :overline nil))))
	 (completions-first-difference ((t (:inherit bold :underline nil))))
	 (cursor                       ((t (:background "black" :overline nil))))

	 ; Interactive search
	 (isearch		 ((t (:background "#f8f893"	 :foreground "#385f38"))))
	 (isearch-fail	 ((t (:background "#5f0000"	 :foreground "#eeeeee" :bold))))
	 (lazy-highlight ((t (:background "#af87ff"	 :foreground "#080808"))))

	 ; Asciidoc mode
	 (markup-complex-replacement-face ((t (:foreground "#afd700"))))
	 (markup-emphasis-face			  ((t (:foreground "cyan"))))
	 (markup-comment-face			  ((t (:foreground "#af5faf"	 :bold))))
	 (markup-gen-face				  ((t (:foreground "#0000ff"))))
	 (markup-reference-face			  ((t (:foreground "#00d7ff"))))
	 (markup-internal-reference-face  ((t (:foreground "#005fff"  :bold))))
	 (markup-list-face				  ((t (:foreground "yellow"	   :bold))))
	 (markup-strong-face			  ((t (:foreground "#5fff00"  :bold))))
	 (markup-typewriter-face		  ((t (:foreground "#d75f00"))))
	 (markup-verbatim-face			  ((t (:foreground "#ff5f00" :bold))))
	 (markup-title-0-face			  ((t (:foreground "#87005f"  :bold))))
	 (markup-title-1-face			  ((t (:foreground "#870087"  :bold))))
	 (markup-title-2-face			  ((t (:foreground "#8700af"  :bold))))
	 (markup-title-3-face			  ((t (:foreground "#8700d7"  :bold))))
	 (markup-title-4-face			  ((t (:foreground "#8700ff"  :bold))))

	 ; Requires linum-mode (line count)
	 (linum	((t (:foreground "#616161"))))

	 ; Requires window-number-mode
	 (window-number-face ((t (:foreground "#d75f00"))))

	 ; Requires yasnippet
	 (yas-field-highlight-face ((t (:foreground "#0087ff" :bold))))

	 ; Makefile colors
	 (makefile-makepp-perl ((t (:background "LightBlue1"))))
	 (makefile-shell	   ((t (:height 1.0))) t)
	 (makefile-space	   ((t (:foreground "#ff87ff" :background "black"))))
	 (makefile-targets	   ((t (:foreground "#af5f00" :bold))))

	 )))

(provide 'alexdantas-color-theme)

