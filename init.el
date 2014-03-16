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
 '(ansi-color-names-vector ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(background-color "#fdf6e3")
 '(background-mode light)
 '(cursor-color "#657b83")
 '(custom-safe-themes (quote ("9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "989b6cb60e97759d7c45d65121f43b746aff298b5cf8dcf5cfd19c03830b83e9" default)))
 '(fci-rule-color "#383838")
 '(foreground-color "#657b83")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(if window-system
	(load-theme 'zenburn t))

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

