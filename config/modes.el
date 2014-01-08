;; Config auto-starting modes

;; As of 12/2013 I'm using `use-package` to manage my emacs modes.
;; It's an awesome extension, check it out on
;; https://github.com/jwiegley/use-package
(require 'use-package)

; `use-package`  Creates autoloads for packages and defer loading
;                until we actually use it.
; `:bind`        Binds key after loading method.
;                Later, use `M-x describe-personal-keybindings`.
; `:mode`        Shortcut for `auto-mode-alist`
; `:interpreter` Shortcut for `auto-interpreter-alist`
; `:init`        Always run - even when not loaded
; `:commands`    Auto loads module when a command is used.
; `:defer`       Forces defer loading.
; `:config`      Runs after module is loaded.

; C-j does ace jump, instant jump anywhere on buffer.
										; Previously C-cSPC.
(use-package ace-jump-mode
  :bind ("C-j" . ace-jump-mode)
  :config
  ;(message "Ace jump mode loaded!")
  )

(use-package markdown-mode
  :mode ("\\.md$" . markdown-mode))

(use-package adoc-mode
  :mode (("\\.adoc$" . adoc-mode)
		 ("\\.asciidoc$" . adoc-mode)
		 ("\\.txt$"  . adoc-mode)))

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :config
  ; please don't convert to CSS at save, kthx
  (setq scss-compile-at-save nil))

(use-package haml-mode
  :defer t
  :config
  ; On HAML we don't have TABs at all
  (;(setq indent-tabs-mode 'nil)
   (define-key haml-mode-map "\C-m" 'newline-and-indent)))

(use-package arduino-mode
  :mode (("\\.pde$" . arduino-mode)
		 ("\\.ino$" . arduino-mode)))

(use-package gas-mode
  :mode (("\\.S\\'" . gas-mode)
		 ("\\.asm$" . gas-mode)))

;; End of `use-package`

; Doxymacs mode SUCKS - adds NOTHING USEFUL

; Workgroups mode
; This is an awesome mode where I can have as much workspaces I want
; and switch dinamically between them
; Read: https://github.com/tlh/workgroups.el
(require 'workgroups)
(workgroups-mode 1)
; Default prefix is C-z
;(setq wg-prefix-key (kbd "C-c z"))

; Disable funky animation when switching workspaces D:
(setq wg-morph-on 'nil)

; Damn flyspell-mode, deactivate it whenever I can
(add-hook 'adoc-mode-hook     '(lambda () (setq flyspell-mode 'nil)))
(add-hook 'markdown-mode-hook '(lambda () (setq flyspell-mode 'nil)))
(setq flyspell-mode 'nil)
(setq flymake-allowed-file-name-masks "")

; Linum mode: Display line numbers on the left
; Set it always, except when using any of the modes listed below
(global-linum-mode nil)
(setq linum-disabled-modes-list
      '(term-mode eshell-mode inferior-haskell-mode calendar-mode
        tetris-mode dired-mode help-mode
       ))

(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
    (linum-mode 1)))

; Linum mode: Make a blank space between line numbers and text
(setq linum-format "%d ")

; YASnippet mode: always needed (but it loads Just-In-Time)
(require 'yasnippet)
(yas-global-mode 1)

;; ; hide-region.el: custom mode to hide blocks of text
;; (load "~/.emacs.d/kure/hide-region.el")

;; ; hide-region.el: setting keyboard shortcuts for hide and unhide regions
;; (global-set-key (kbd "C-c h r") 'hide-region-hide)
;; (global-set-key (kbd "C-c h u") 'hide-region-unhide)

; Highlight spaces/tabs, see 'init/keybindings.el'
;(load "~/.emacs.d/custom/hilite-chars.el")

; Haskell mode
(defun ghci-on-new-buffer ()
  "Launch a new ghci on a new buffer"
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (ansi-term "/usr/bin/ghci"))

(add-hook 'haskell-mode-hook
          (lambda ()
            ; A new ghci term with 'C-c C-g'
            (local-set-key (kbd "C-c C-g") 'ghci-on-new-buffer)

            ; Overwrite common indentation, to make haskell's work
;           (local-set-key (kbd "RET") 'newline)

            ; Haskell has it's own indentation stuff
            (turn-on-haskell-indentation)))

; AUCTeX mode: assuming it is installed through ELPA
;(load "auctex.el" nil t t)
(add-hook 'TeX-mode-hook
          (lambda ()
            (setq TeX-auto-save t)
            (setq TeX-parse-self t)
            (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
            (local-set-key (kbd "C-j") 'TeX-newline)
			(setq TeX-view-program-list '("llpp" "llpp %o"))
            (TeX-PDF-mode)))

; Window number mode: Switch between windows
(autoload 'window-number-mode "window-number" t)
(window-number-mode)

; Window number mode: Use Meta as prefix for changing windows
(window-number-meta-mode)

;; Assembler mode
; * Make my default comment char to '#' instead of ';' because
;   of MIPS.
; * Reverted back to ';' because of IA-32/x86.
;(setq asm-comment-char 35)

; Smart parenthesis up your ass.
; (what does it do?)
(smartparens-mode)

; Ido mode is awesome!
; I want it for everything!
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

; Makes Ido guess the context of 'Find File at Point'
(setq ido-use-filename-at-point 'guess)

