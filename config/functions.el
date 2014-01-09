;; A collection of useful functions I've accumulated
;; throughout the years...

; Interesting function I saw here:
; http://blog.burtcorp.com/post/32196269136/emacs-the-white-box
(defun comment-and-duplicate-line ()
  "Copy current line to line below and comment current line."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (line (buffer-substring-no-properties beg end))
         (column (current-column)))
    (comment-region beg end)
    (goto-char (line-end-position))
    (newline)
    (insert line)
    (move-to-column column)))

; Asciidoc shortcut to preview current buffer in HTML
(defun adoc-preview-html ()
  "Previews current Asciidoc buffer in HTML on defaut web browser."
  (interactive)
  (let ((thefile (current-buffer))
        (thetmpfile ("/tmp/file.html"))))
  (shell-command (concat
                  (concat ("asciidoc -o ") 'thetmpfile)
                  'thefile))
  (browse-url
   (concat ("file://") 'thetmpfile)))

(defun query-friend-name (x)
  "…"
  (interactive "sEnter friend's name: ")
  (message "Name: %s" x))

; Creates a new C++ class (.cpp/.hpp) with default template.
(defun new-class-cpp (name)
  "Insert a C++ class definition."
  (interactive "sClass name: ")
  (let* ((header-file-name (concat name ".hpp"))
         (header-include-string (upcase (concat name "_H_DEFINED")))
         (def-file-name    (concat name ".cpp")))

    ;; create HPP file
    (set-buffer (get-buffer-create header-file-name))
    (set-visited-file-name header-file-name)
    (c++-mode)
    (turn-on-font-lock)
    (insert (concat
             "#ifndef " header-include-string "\n"
             "#define " header-include-string "\n"
             "\n"
             "///\n"
             "class " name "\n"
             "{\n"
             "public:\n"
             name "();\n"
             "virtual ~" name "();\n\n"
             "private:\n"
             "};"
             "\n\n#endif //" header-include-string "\n\n"))
    (beginning-of-buffer)
    (while (and (not (eobp)) (forward-line))
      (indent-according-to-mode))

    ;; create CPP file
    (set-buffer (get-buffer-create def-file-name))
    (set-visited-file-name def-file-name)
    (switch-to-buffer (current-buffer))
    (c++-mode)
    (turn-on-font-lock)
    (insert (concat
             "#include \"" header-file-name "\"\n\n"
             name "::" name "()\n"
             "{\n\n}\n"
             name "::~" name "()\n"
             "{\n\n}\n\n"))
    (beginning-of-buffer)
    (while (and (not (eobp)) (forward-line))
      (indent-according-to-mode))
    (beginning-of-buffer)))

; Beautiful method to instantly access any buffer
; Based on ace-jump
; http://blog.waymondo.com/2013-04-07-quick-emacs-buffer-switching-with-ace-jump-mode/

(require 'bs)
(require 'ace-jump-mode)
(require 'perspective)

; Simple layout, increase window height
(setq bs-max-window-height 30
      bs-attributes-list (quote ((" " 2 2 left " ")
                                 (" " 1 1 left bs--get-marked-string)
                                 (" " 1 1 left " ")
                                 ("Buffer" bs--get-name-length 10 left bs--get-name))))

; Filter buffers to current perspective
(add-to-list 'bs-configurations
             '("PERSP" nil nil nil
               (lambda (buf)
                 (with-current-buffer buf
                   (not (member buf (persp-buffers persp-curr)))))) nil)

; On end of ace jump, select buffer
(defun bs-ace-jump-end-hook ()
  (if (string-match (buffer-name) "*buffer-selection*")
      (bs-select)))

(add-hook 'ace-jump-mode-end-hook 'bs-ace-jump-end-hook)

(defun hemacs-jump-buff ()
  (interactive)
  (bs--show-with-configuration "PERSP")
  (setq ace-jump-mode-scope 'window)
  (beginning-of-buffer)
  (call-interactively 'ace-jump-line-mode))

(provide 'hemacs-jump-buff)

; Y U NO WORK
(defun indent-line ()
  "Indents the current line"
  (interactive)
  (beginning-of-line)
  (set-mark-command)
  (end-of-line)
  (indent-region))


; Shortcuts to switch between TAB/Spaces indentation.
(defun tabify-whole-buffer ()
  "Replaces each `tab-width` spaces on the whole buffer with a TAB"
  (interactive)
  (tabify (point-min) (point-max)))
;(add-hook 'before-save-hook 'tabify-whole-buffer)

(defun untabify-whole-buffer ()
  "Replaces each `tab` with `tab-width` spaces"
  (interactive)
  (untabify (point-min) (point-max)))

; Now paragraph will be filled till 80 chars (M-q)
(defun change-default-fill-column ()
  (setq fill-column 80))
(add-hook 'fundamental-mode-hook 'change-default-fill-column)
(add-hook 'text-mode-hook 'change-default-fill-column)

; When splitting windows, go to next buffer
(defadvice split-window-vertically
  (after my-window-split first () activate)
  (set-window-buffer (next-window) (other-buffer)))
(defadvice split-window-horizontally
  (after my-window-split first () activate)
  (set-window-buffer (next-window) (other-buffer)))

; Open custom files easily. Thanks, Xah (ergoemacs.org)!
(defun kure-open-file-fast (openCode)
  "Prompt to open a file from a pre-defined set"
  (interactive "sOpen fIle 1::emacs 2::bashrc 3::bash_aliases: ")
  (let (file)
	(setq file
		  (cond
		   ((string= openCode "1") "~/.emacs.d/init.el" )
		   ((string= openCode "2") "~/.bashrc" )
		   ((string= openCode "3") "~/.bash_aliases" )
		   (t (error "Invalid option %s." openCode))
		   ))
	(find-file file)))

