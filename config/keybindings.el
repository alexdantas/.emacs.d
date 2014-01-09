;; Global and local keybindigs.

; Make C-w delete previous word.
; Make C-xC-k kill region.
; This makes C-w consistent between emacs and the shell.
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

; Will never be the same without Ace Jump
(global-set-key (kbd "C-j") 'ace-jump-mode)

; Multi-cursors at once! Farewell, Sublime Text.
; Just mark a region and let the magic happen!
(require 'multiple-cursors)
;(global-set-key (kbd "C-c m") 'mc/edit-lines)
(global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c =") 'mc/mark-all-like-this)

; C-cs replace strings.
(global-set-key (kbd "C-c s") 'replace-string)

; Another handy shortcut for compiling projects
(global-set-key (kbd "C-c c") 'compile)
; C-x ` GOES TO THE NEXT ERROR MESSAGE! W00w
(global-set-key (kbd "C-c g") 'gdb)

; Now access bookmarks with 'C-c b'
(global-set-key (kbd "C-c b") 'bookmark-bmenu-list)

; Keybinding to show recently opened files
(global-set-key (kbd "C-c r") 'recentf-open-files)

; Interactivelly resize/create/delete windows with arrow keys
(global-set-key (kbd "C-c w") 'windresize)

; We'll always try to indent when newlinin'
(global-set-key (kbd "RET") 'newline-and-indent)

; Toggle line numbers on 'C-c l' (linum mode)
(global-set-key (kbd "C-c l") 'linum-mode)

; highlight-chars.el: Binds <F12> to toggle
; highlight trailing whitespace and <F9> to toggle tabs
(global-set-key (kbd "<f12>") 'hc-toggle-highlight-trailing-whitespace)
(global-set-key (kbd "<f9>")  'hc-toggle-highlight-tabs)

; A new shell emulator with F1
(global-set-key [f1] '(lambda ()
						(interactive)
						(split-window-horizontally)
						(other-window 1)
						(ansi-term "/bin/bash")))

; Kill current buffer/window with F2
(global-set-key [f2] '(lambda ()
						(interactive)
						(delete-window)))
; M-x used to call 'execute-extended-command'
; Now it does smex, an enhancement using
; ido-like completions
; Also, Do C-xC-m instead of M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key "\C-x\C-m" 'smex)

; Makes easier to switch buffers.
(global-set-key (kbd "M-`") 'ido-switch-buffer)

; Hooray! Aligning texts got wayy easier!
; Just type the char you want to align your text to
(global-set-key (kbd "C-c a") 'align-regexp)

; My custom function to comment this line and
; duplicate it below.
(global-set-key (kbd "C-c C-d") 'comment-and-duplicate-line)

; Magit
(global-set-key (kbd "C-c m") 'magit-status)

; Keybindings for CEDET (Collection of Emacs Development Tools)
; followed by a hook to only activate them at the right time
; (on C/C++ sources).
(defun my-cedet-hook ()
  "My keybindings for CEDET"
  (global-set-key [(control return)] 'semantic-ia-complete-symbol)
  (global-set-key "\C-c?"   'semantic-ia-complete-symbol-menu)
  (global-set-key "\C-c>"   'semantic-complete-analyze-inline)
  (global-set-key "\C-cp"   'semantic-analyze-proto-impl-toggle)
;  (global-set-key "."       'semantic-complete-self-insert)
;  (global-set-key ">"       'semantic-complete-self-insert)
  (global-set-key (kbd "C-c TAB") 'semantic-ia-complete-symbol)
  (global-set-key (kbd "C-c >"  ) 'semantic-complete-analyze-inline)
  (global-set-key (kbd "C-c j"  ) 'semantic-ia-fast-jump)
  (global-set-key (kbd "C-c d"  ) 'semantic-ia-show-doc)
  ;(global-set-key (kbd "C-c m"  ) 'semantic-ia-show-summary)
  (global-set-key (kbd "C-c p"  ) 'semantic-analyze-proto-impl-toggle)
  (global-set-key (kbd "C-c g"  ) 'semantic-symref-symbol)
  (global-set-key (kbd "C-c C-r") 'semantic-symref))

;(add-hook 'c-mode-common-hook 'my-cedet-hook)

; Dired mode: Keybindings to ease navegation
; Prevents dired from spawning a buffer for each directory
; (thanks, 'http://ergoemacs.org/emacs/emacs_dired_tips.html'!)
(add-hook 'dired-mode-hook
		  (lambda ()
			(define-key dired-mode-map (kbd "C-b")
			  (lambda () (interactive) (find-alternate-file "..")))
			(define-key dired-mode-map (kbd "^")
			  (lambda () (interactive) (find-alternate-file "..")))
			(define-key dired-mode-map (kbd "C-f")
			  'dired-find-alternate-file)
			(define-key dired-mode-map (kbd "<return>")
			  'dired-find-alternate-file)))

; Custom function to open pre-defined files easily.
; See 'functions.el`.
(global-set-key (kbd "C-c o") 'kure-open-file-fast)

; Tab is for Autocomplete
; C-o is for Yasnippet
(global-set-key (kbd "C-o") 'yas-expand)

(global-set-key (kbd "TAB") 'indent-for-tab-command)


