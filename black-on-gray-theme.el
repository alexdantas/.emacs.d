(deftheme black-on-gray
  "black-on-gray theme")

(set-default-font "Ubuntu Mono 12")


(custom-theme-set-faces
 'black-on-gray

 '(default ((t (:background "#d8d8d8" :foreground "#1a1a1a"))))
 '(border  ((t (:foregound "blue"))))

 ; What is this?
 '(fringe ((t (:background "black"))))

 ; Line before minibuffer
 '(mode-line                                                  ((t (:foreground "#5f5f5f" :background "#ccc" :bold t :box (:line-width 2 :style released-button)))))

 '(mode-line-inactive  ((t (:foreground "#5f5f5f" :background "#cccccc"))))
 '(mode-line-buffer-id ((t (:background "white"))))
 '(mode-line-emphasis  ((t (:background "#000"	:bold t))))
 '(mode-line-highlight ((t (:foreground "#550000"	:bold t))))

 ; Current line
 '(hl-line                                                    ((t (:background "default")))) ; same as background

 ; Selected Text
 '(region                                                     ((t (:background "#00002a"))))

 ; Misc. programming stuff
 '(font-lock-builtin-face                                     ((t (:foreground "#2a55d4"   :bold t ))))
 '(font-lock-comment-delimiter-face                           ((t (:foreground "#d42a00"           ))))
 '(font-lock-comment-face                                     ((t (:foreground "#d42a00"           ))))
 '(font-lock-constant-face                                    ((t (:foreground "#555500"   :bold t ))))
 '(font-lock-doc-face                                         ((t (:foreground "#aa002a"           ))))
 '(font-lock-function-name-face                               ((t (:foreground "#802a80"   :bold t ))))
 '(font-lock-keyword-face                                     ((t (:foreground "#0080d4"   :bold t ))))
 '(font-lock-negation-char-face                               ((t (:foreground "#d4aa00"   :bold t ))))
 '(font-lock-preprocessor-face                                ((t (:foreground "#008000"   :bold t ))))
 '(font-lock-string-face                                      ((t (:foreground "#008000"   :bold t ))))
 '(font-lock-type-face                                        ((t (:foreground "#0055d4"   :bold t ))))
 '(font-lock-variable-name-face                               ((t (:foreground "#444"  :bold t ))))
 '(font-lock-warning-face                                     ((t (:foreground "#f00"      :bold t ))))
 '(show-paren-match                                           ((t (:foreground "#2ad480"   :bold t ))))
 '(show-paren-mismatch                                        ((t (:foreground "#552a00"   :bold t ))))

 '(custom-link ((t (:foreground "blue" :underline t))))

 ; Le minibuffer
 '(minibuffer-prompt                                          ((t (:foreground "#00aa00"   :bold t ))))

 ; C/C++ modes
 '(c-annotation-face                                          ((t (:inherit font-lock-constant-face :underline nil))))
 '(c-nonbreakable-space-face                                  ((t (:background "brightred" :underline nil :weight bold))) t)
 '(completions-annotations                                    ((t (:inherit italic :slant normal))))
 '(completions-common-part                                    ((t (:inherit default :overline nil))))
 '(completions-first-difference                               ((t (:inherit bold :underline nil))))
 '(cursor                                                     ((t (:background "black" :overline nil))))

 ; Interactive search
 '(isearch                                                    ((t (:background "#d4002a"	 :foreground "#eeeeee"))))
 '(isearch-fail                                               ((t (:background "#2a0000"	 :foreground "#eeeeee" :bold t))))
 '(lazy-highlight                                             ((t (:background "#8055d4"	 :foreground "#080808"))))

 ; ido
 '(ido-first-match ((t (:foreground "default" :box (:line-width 3 :style released-button)))))

 '(ido-only-match ((t (:foreground "default" :bold t ))))
 '(ido-subdir     ((t (:foreground "#d42a00" :bold t ))))

 ; Asciidoc mode
 '(markup-complex-replacement-face ((t (:foreground "#80aa00"))))
 '(markup-emphasis-face            ((t (:foreground "cyan"))))
 '(markup-comment-face             ((t (:foreground "#802a80"	 :bold t))))
 '(markup-gen-face                 ((t (:foreground "#0000d4"))))
 '(markup-reference-face           ((t (:foreground "#00aad4"))))
 '(markup-internal-reference-face  ((t (:foreground "#002ad4"  :bold t))))
 '(markup-list-face                ((t (:foreground "yellow"	   :bold t))))
 '(markup-strong-face              ((t (:foreground "#2ad400"  :bold t))))
 '(markup-typewriter-face          ((t (:foreground "#aa2a00"))))
 '(markup-verbatim-face            ((t (:foreground "#d42a00" :bold t))))
 '(markup-table-face               ((t (:foreground "#800000"))))
 '(markup-meta-face                ((t (:foreground "#800000" :bold t))))
 '(markup-code-face                ((t (:foreground "#d42a00"))))
 '(markup-meta-hide-face           ((t (:foreground "#626262"))))
 ;	 (markup-gen-face              ((t (:foreground "red" :bold t))))
 ;WHY DOES THIS NOT OWRK
 '(markup-title-0-face             ((t (:foreground "#d455d4"  :bold t))))
 '(markup-title-1-face             ((t (:foreground "#d455aa"  :bold t))))
 '(markup-title-2-face             ((t (:foreground "#d45580"  :bold t))))
 '(markup-title-3-face             ((t (:foreground "#d45555"  :bold t))))
 '(markup-title-4-face             ((t (:foreground "#d4552a"  :bold t))))

 ; Requires linum-mode (line count)
 '(linum                                                      ((t (:foreground "black" :background "#c6c6c6"))))

 ; Requires window-number-mode
 '(window-number-face                                         ((t (:foreground "#aa2a00"))))

 ; Requires yasnippet
 '(yas-field-highlight-face                                   ((t (:foreground "#0055d4" :bold t))))

 ; Makefile colors
 '(makefile-makepp-perl                                       ((t (:background "LightBlue1"))))
 '(makefile-shell                                             ((t (:height 1.0))) t)
 '(makefile-space                                             ((t (:foreground "#d455d4" :background "black"))))
 '(makefile-targets                                           ((t (:foreground "#802a00" :bold t))))

 '(custom-visibility ((t (:foreground "orange" :underline t))))

 '(link ((t (:foreground "blue" :underline t))))
 '(link-visited ((t (:foreground "purple" :underline t))))

 ;; ; '(blue                                      ((t (:foreground "blue"))))
 ;;  '(bold                                      ((t (:bold t :size "10pt" :foreground "blue"))))
 ;;  '(bold-italic                               ((t (:italic t :bold t :size "10pt"))))
 ;;  '(border-glyph                              ((t (:size "11pt"))))
 ;;  '(buffers-tab                               ((t (:background "gray75"))))
 ;;  '(buffers-tab-face                          ((t (:background "gray75"))))
 ;;  '(display-time-mail-balloon-enhance-face    ((t (:background "orange"))))
 ;;  '(display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))
 ;;  '(display-time-time-balloon-face            ((t (:foreground "red"))))
 ;;  '(ecb-bucket-token-face                     ((t (:bold t :size "10pt"))))
 ;;  '(ecb-default-general-face                  ((t (nil))))
 ;;  '(ecb-default-highlight-face                ((t (:background "cornflower blue" :foreground "yellow"))))
 ;;  '(ecb-directories-general-face              ((t (nil))))
 ;;  '(ecb-directory-face                        ((t (:background "cornflower blue" :foreground "yellow"))))
 ;;  '(ecb-history-face                          ((t (:background "cornflower blue" :foreground "yellow"))))
 ;;  '(ecb-history-general-face                  ((t (nil))))
 ;;  '(ecb-method-face                           ((t (:background "cornflower blue" :foreground "yellow"))))
 ;;  '(ecb-methods-general-face                  ((t (nil))))
 ;;  '(ecb-source-face                           ((t (:background "cornflower blue" :foreground "yellow"))))
 ;;  '(ecb-source-in-directories-buffer-face     ((t (:foreground "medium blue"))))
 ;;  '(ecb-sources-general-face                  ((t (nil))))
 ;;  '(ecb-token-header-face                     ((t (:background "SeaGreen1"))))
 ;;  '(ecb-type-token-class-face                 ((t (:bold t :size "10pt"))))
 ;;  '(ecb-type-token-enum-face                  ((t (:bold t :size "10pt"))))
 ;;  '(ecb-type-token-group-face                 ((t (:bold t :size "10pt" :foreground "dimgray"))))
 ;;  '(ecb-type-token-interface-face             ((t (:bold t :size "10pt"))))
 ;;  '(ecb-type-token-struct-face                ((t (:bold t :size "10pt"))))
 ;;  '(ecb-type-token-typedef-face               ((t (:bold t :size "10pt"))))

 ;;  '(font-lock-builtin-face                    ((t (:foreground "#2a55d4" :bold t))))
 ;;  '(font-lock-constant-face                   ((t (:foreground "blue3" :bold t))))
 ;;  '(font-lock-comment-delimiter-face ((t (:foreground "#d42a00"))))
 ;;  '(font-lock-comment-face		   ((t (:foreground "#d42a00"  ))))
 ;;  '(font-lock-doc-face                        ((t (:foreground "green4"))))
 ;;  '(font-lock-doc-string-face                 ((t (:foreground "green4"))))
 ;;  '(font-lock-function-name-face              ((t (:foreground "orange"))))
 ;;  '(font-lock-keyword-face                    ((t (:foreground "orange"))))
 ;;  '(font-lock-preprocessor-face               ((t (:foreground "blue3"))))
 ;;  '(font-lock-reference-face                  ((t (:foreground "red3"))))
 ;;  '(font-lock-string-face                     ((t (nil))))
 ;;  '(font-lock-type-face                       ((t (nil))))
 ;;  '(font-lock-variable-name-face              ((t (nil))))
 ;;  '(font-lock-warning-face                    ((t (nil))))

 ;;  '(green                                     ((t (:foreground "green"))))
 ;;  '(gui-button-face                           ((t (:background "grey75"))))
 ;;  '(gui-element                               ((t (:size "8pt" :background "gray75"))))
 ;;  '(highlight                                 ((t (:background "darkseagreen2"))))
 ;;  '(isearch                                   ((t (:background "paleturquoise"))))
 ;;  '(isearch-secondary                         ((t (:foreground "red3"))))
 ;;  '(italic                                    ((t (:size "10pt"))))
 ;;  '(left-margin                               ((t (nil))))
 ;;  '(list-mode-item-selected                   ((t (:background "gray68"))))
 ;;  '(modeline                                  ((t (:background "gray75"))))
 ;;  '(modeline-buffer-id                        ((t (:background "gray75" :foreground "blue4"))))
 ;;  '(modeline-mousable                         ((t (:background "gray75" :foreground "firebrick"))))
 ;;  '(modeline-mousable-minor-mode              ((t (:background "gray75" :foreground "green4"))))
 ;;  '(paren-blink-off                           ((t (:foreground "gray"))))
 ;;  '(paren-match                               ((t (:background "darkseagreen2"))))
 ;;  '(paren-mismatch                            ((t (nil))))
 ;;  '(pointer                                   ((t (nil))))
 ;;  '(primary-selection                         ((t (:background "gray65"))))
 ;;  '(red                                       ((t (:foreground "red"))))
 ;;  '(region                                    ((t (:background "gray65"))))
 ;;  '(right-margin                              ((t (nil))))
 ;;  '(secondary-selection                       ((t (:background "paleturquoise"))))
 ;;  '(semantic-dirty-token-face                 ((t (nil))))
 ;;  '(semantic-unmatched-syntax-face            ((t (nil))))
 ;;  '(text-cursor                               ((t (:background "red" :foreground "gray"))))
 ;;  '(toolbar                                   ((t (:background "gray75"))))
 ;;  '(underline                                 ((t (:underline t))))
 ;;  '(vertical-divider                          ((t (:background "gray75"))))
 ;;  '(widget                                    ((t (:size "8pt" :background "gray75"))))
 ;;  '(widget-button-face                        ((t (:bold t))))
 ;;  '(widget-button-pressed-face                ((t (:foreground "red"))))
 ;;  '(widget-documentation-face                 ((t (:foreground "dark green"))))
 ;;  '(widget-field-face                         ((t (:background "gray85"))))
 ;;  '(widget-inactive-face                      ((t (nil))))
 ;;  '(yellow                                    ((t (:foreground "yellow"))))
 ;;  '(zmacs-region                              ((t (:background "gray65"))))
 ;; 	 ; Requires linum-mode (line count)
 ;;  '(linum                                     ((t (:foreground "#default" :background "#default"))))

 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'black-on-gray)

;;; black-on-gray-theme.el ends here
