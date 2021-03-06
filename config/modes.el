;; Config auto-starting modes

;; As of 12/2013 I'm using `use-package` to manage my emacs modes.
;; It's an awesome extension, check it out on
;; https://github.com/jwiegley/use-package
(require 'use-package)

; Le `use-package` cheatsheet
;
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



(use-package ace-jump-mode
  ; I'm using a different file to store key bindings
  ; :bind ("C-j" . ace-jump-mode)
  )

(use-package markdown-mode
  :mode ("\\.md$" . markdown-mode))

; Aww yeah, Arch Linux!
(use-package shell-script-mode
  :mode ("\\PKGBUILD$" . shell-script-mode))

(use-package adoc-mode
  :mode (("\\.adoc$" . adoc-mode)
		 ("\\.asciidoc$" . adoc-mode)
		 ("\\.txt$"  . adoc-mode)))

(use-package gnuplot-mode
  ;; I had trouble finding a "standard" gnuplot
  ;; file extension.
  :mode (("\\.gnuplot$" . gnuplot-mode)
		 ("\\.gp$" . gnuplot-mode)
		 ("\\.gpi$" . gnuplot-mode)
		 ("\\.plt$" . gnuplot-mode)))

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :config
  ; Don't convert to CSS at save, kthx
  (setq scss-compile-at-save nil))

(use-package haml-mode
  :defer t
  :config
  ; On HAML we don't have TABs at all
  ;(setq indent-tabs-mode 'nil)
  (define-key haml-mode-map "\C-m" 'newline-and-indent))

(use-package arduino-mode
  :mode (("\\.pde$" . arduino-mode)
		 ("\\.ino$" . arduino-mode)))

(use-package gas-mode
  :mode (("\\.S\\'" . gas-mode)
		 ("\\.asm$" . gas-mode)))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)))

; Omitting files starting with dots (except for "..")
; Remember that's M-o to toggle auto-hiding.
; Source: http://stackoverflow.com/a/14850863/1094964
(use-package dired-x
  :config
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files "^\\.[^.]"))

; Customizing `recentf`
(use-package recentf
  :config
  (setq recentf-max-saved-items 50))

; Uniquify removes that awful default naming scheme for
; same file names.
; Instead of `Makefile<1>` and `Makefile<2>` we get
; `Makefile:parent_folder` and `Makefile:other_parent_folder`
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator ":"))

;; End of `use-package`

;; Mapping modes to be opened when reading specific files
(add-to-list 'auto-mode-alist
			 '(".inputrc" . shell-script-mode)
			 '(".bashrc"  . shell-script-mode)
;			 '(".bash\\.aliases" . shell-script-mode)
)

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

; Loving flyspell-mode for text-related modes
(defun turn-on-flyspell ()
  (flyspell-mode 1)
  (flyspell-buffer))

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'adoc-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

; Deactivating it whenever I can
;(setq flyspell-mode 'nil)
;(setq flymake-allowed-file-name-masks "")

; I love Sr-Speedbar, a nice sidebar for Emacs
; Great for IDE stuff
; http://www.emacswiki.org/emacs/SrSpeedbar
(defun sr-speedbar-custom-options ()
  (interactive)
  ; I don't want it to change every time
  ; I switch buffers
  (sr-speedbar-refresh-turn-off))

(add-hook 'sr-speedbar-mode-hook 'sr-speedbar-custom-options)

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
(setq yas-snippet-dirs "~/.emacs.d/yasnippets")
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

; Customizations for the big Web Mode
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)

  ; Syntax highlighting
  (set-face-attribute 'web-mode-doctype-face          nil :foreground "#5f8700")
  (set-face-attribute 'web-mode-html-tag-face         nil :foreground "#af8700")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#626262")
  (set-face-attribute 'web-mode-html-attr-name-face   nil :foreground "#d75f00")
  (set-face-attribute 'web-mode-html-attr-value-face  nil :foreground "#af8700" :bold)
  (set-face-attribute 'web-mode-html-attr-equal-face  nil :foreground "#626262")
  (set-face-attribute 'web-mode-html-tag-custom-face  nil :foreground "#ffffff")
  (set-face-attribute 'web-mode-attr-tag-custom-face  nil :foreground "#ffffff"))
(add-hook 'web-mode-hook 'my-web-mode-hook)

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

; My favorite mode for editing Javascript!
(add-hook 'js2-mode-hook
		  (lambda ()
			; Damn it's default setting of ENTER key.
			(define-key js2-mode-map (kbd "RET") 'newline-and-indent)))

; Window number mode: Switch between windows
(autoload 'window-number-mode "window-number" t)
(window-number-mode)

; Window number mode: Use Meta as prefix for changing windows
(window-number-meta-mode)

;; Assembler mode
; * Make my default comment char to '#' instead of ';' because
;   of MIPS.
; * Reverted back to ';' because of IA-32/x86.
(setq asm-comment-char 35)

; Smart parenthesis up your ass.
; (what does it do?)
(smartparens-mode)

; Ido mode is awesome!
; I want it for everything!
(ido-mode 1)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

; Makes Ido guess the context of 'Find File at Point'
(setq ido-use-filename-at-point 'guess)

; Clang Complete Async (on elpa)
(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/elpa/auto-complete-clang-async-server/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process))

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)

; Dired mode: switches passed to 'ls' when viewing dired mode
(setq dired-listing-switches "-lahpD")

; Dired details hides unnecessary things from Dired mode
(require 'dired-details+)

; Customizing ibuffer's options
;
; Source:
; http://xsteve.at/prg/emacs/power-user-tips.html
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)

;; THIS GIVES AN INITIALIZATION ERROR
;; FIND OUT WHY
;;
;; ;; On ibuffer, use human readable Size column instead of original one
;; (define-ibuffer-column size-h
;;   (:name "Size" :inline t)
;;   (cond
;;    ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
;;    ((> (buffer-size) 1000)    (format "%7.1fK" (/ (buffer-size) 1000.0)))
;;    (t (format "%8d" (buffer-size)))))
;;
;; ;; Modify the default ibuffer-formats
;; (setq ibuffer-formats
;; 	  '((mark modified read-only " "
;; 			  (name 18 18 :left :elide)
;; 			  " "
;; 			  (size-h 9 -1 :right)
;; 			  " "
;; 			  (mode 16 16 :left :elide)
;; 			  " "
;; 			  filename-and-process)))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
	ad-do-it
	(ibuffer-jump-to-buffer recent-buffer-name)))

(ad-activate 'ibuffer)

;; This makes emacs save the cursor position when opening
;; all files.
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace.el"))
(setq-default save-place t)

;; My own Dogescript mode!
;; https://github.com/alexdantas/dogescript-mode
(load "~/.emacs.d/local/dogescript-mode.el")

