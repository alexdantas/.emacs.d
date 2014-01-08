;; General emacs config stuff.

; Don't make backup files (file~)
(setq make-backup-files nil)

; Don't make auto-saves (#file#)
(setq auto-save-default nil)

; Don't fucking blink my screen when something goes wrong, it hurts my eyes.
(setq ring-bell-function 'ignore)

; My custom bookmarks file
(setq bookmark-default-file "~/.emacs.d/bookmarks")

; Keep a list of my 10 recently opened files
(recentf-mode 1)

; Avoid the emacs' default init screen and...
;(setq inhibit-startup-screen t)

; ...replace it with my own
;(find-file "~/org/sandbox.org")

; Sets the maximum size of the kill ring. I don't know why I did this.
(setq kill-ring-max 500)

; When exiting the terminal, close it's buffer and window
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
	  (let ((buffer (process-buffer proc)))
		ad-do-it
		(kill-buffer buffer)
		(delete-window))
	ad-do-it))
(ad-activate 'term-sentinel)

; Force term to use UTF-8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

; Disable the menubar. Set arg >= 1 to re-enable
(menu-bar-mode 0)
(setq menu-bar-mode 'nil)

; Always show matched parentheses with custom colors
(show-paren-mode 1)
(setq show-paren-mode 't)

(setq tool-bar-mode 'nil)

