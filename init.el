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

; How much time emacs took to intialize.
; * I'm still at 4.2 seconds, dang. (half 2013)
; * W00t, now got to 2.6 seconds! (begin 2014)
;
(message "* So far Emacs loaded in %fs"
         (* 0.000001 (apply #'-
                            (mapcar (lambda (time)
                                      (+ (* 1000000 (+ (* 65536 (first time))
                                                       (second time))) (third time)))
                                    (list (current-time) before-init-time)))))

;; What is this?
;; (put 'downcase-region 'disabled nil)

