;; Editing settings

; Font lock mode: By default colorize stuff
(global-font-lock-mode t)

; My custom hook for C/C++ modes it
(defun my-c-mode-hook ()
  (local-set-key (kbd "C-c d") 'ff-find-other-file) ; find .hpp
  (c-toggle-auto-newline 1)) ; Add newline when pressed ';'
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

; Preferred identation style, with 4 spaces
(setq c-default-style "bsd"
      c-basic-offset 4)

; Trying to fix Smart Tabs on emacs
(setq cua-auto-tabify-rectangles nil)

(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
    ad-do-it))
(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
        (indent-tabs-mode
         (save-excursion
           (beginning-of-line)
           (while (looking-at "\t*\\( +\\)\t+")
             (replace-match "" nil nil nil 1)))
         (setq tab-width tab-width)
         (let ((tab-width fill-column)
               (,offset fill-column)
               (wstart (window-start)))
           (unwind-protect
               (progn ad-do-it)
             (set-window-start (selected-window) wstart))))
        (t
         ad-do-it)))))

(smart-tabs-advice c-indent-line c-basic-offset)
    (smart-tabs-advice c-indent-region c-basic-offset)

; Default tab width is 4 spaces
(setq default-tab-width 4)

; Instead of inserting TABs, make sure TAB will ALWAYS indent line
(setq tab-always-indent 't)

; Insert the actual TAB character
;(global-set-key (kbd "TAB") 'self-insert-command)

; Always delete trailing whitespace before saving
; But don't delete trailing lines
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq delete-trailing-lines 'nil)

; Activate gdb's many windows mode (see help on 'gdb')
(setq gdb-many-windows t)

; Let's disable some stuff on dired
(put 'dired-find-alternate-file 'disabled nil)

; Force emacs to follow git symlinks (don't know what this is)
(setq vc-follow-symlinks t)

;(add-hook 'fundamental-mode-hook 'auto-complete-mode)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
                  "~/.emacs.d/elpa/auto-complete-20130503.2013/dict")
(ac-config-default)
(setq ac-ignore-case nil)
(setq ac-comphist-file (expand-file-name
      "~/.emacs.d/kure/ac-comphist.dat"))

