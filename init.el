;; First file loaded by Emacs
;;
;; I've replaced all calls to 'require' for 'autoload',
;; hoping it might speed the initialization time. So far so good.
;;
;; Everything on ~/.emacs.d/elpa/ directory is automagically loaded.
;; I just need to call it's functions when necessary.

;; Daemons
; Starting an emacs daemon boosts up starting time a lot and makes
; buffer sharing between emacs instances possible.
; The following makes multiple emacs daemons possible.
; Refer to:
; http://tychoish.com/rhizome/running-multiple-emacs-daemons-on-a-single-system/
;(setq server-use-tcp t)

; Loading/initializing the Emacs Package Archive
(load "~/.emacs.d/config/elpa.el")

;; ErgoEmacs
;; (setq ergoemacs-theme nil)				; Standard ErgoEmacs Theme
;; (setq ergoemacs-keyboard-layout "us")	; QWERTY Keyboard Layout
;; (ergoemacs-mode 1)

;; Evil
;; (evil-mode 1)
;; (define-key evil-normal-state-map "gj" 'ace-jump-mode)

; Custom functions and clever hacks.
(load "~/.emacs.d/config/functions.el")

; Configs for general emacs usage, usually meta configurations.
(load "~/.emacs.d/config/general.el")

; General things for everyday editing
; Lots of mode-specific hacking here.
(load "~/.emacs.d/config/editing.el")

; Configuring and auto-starting modes
(load "~/.emacs.d/config/modes.el")

; All my saved macros.
(load "~/.emacs.d/config/macros.el")

; Appearance of the editor
(load "~/.emacs.d/config/interface.el")

; Transforming emacs into a full-fledged IDE for (primarily) C/C++.
; Messing with CEDET and auto-completing.
;(load "~/.emacs.d/config/ide.el")

; My custom keybindings
(load "~/.emacs.d/config/keybindings.el")

; Local `.el` files
(add-to-list 'load-path "~/.emacs.d/local/")

; Load settings only if on graphical emacs
(if window-system
	(load "~/.emacs.d/config/gui.el"))

;; What is this?

(put 'downcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list (quote (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber") ("View" "llpp %s.pdf" TeX-run-discard-or-function t t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(ansi-color-names-vector ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(background-color "#fdf6e3")
 '(background-mode light)
 '(cursor-color "#657b83")
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "95970c3221da88c891809a78a2605f732104b42ea7411046c8b79c1dad81b85f" "0f0e3af1ec61d04ff92f238b165dbc6d2a7b4ade7ed9812b4ce6b075e08f49fe" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "989b6cb60e97759d7c45d65121f43b746aff298b5cf8dcf5cfd19c03830b83e9" default)))
 '(fci-rule-color "#383838")
 '(foreground-color "#657b83")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

; How much time emacs took to intialize.
;
; * I'm still at 4.2 seconds, dang. (half 2013)
; * W00t, now got to 2.6 seconds! (begin 2014)
;
(message "* So far Emacs loaded in %fs"
         (* 0.000001 (apply #'-
                            (mapcar (lambda (time)
                                      (+ (* 1000000 (+ (* 65536 (first time))
                                                       (second time))) (third time)))
                                    (list (current-time) before-init-time)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
