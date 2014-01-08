
; Helper for using during macro definitions.
(defun my-macro-query (arg)
  "Prompt for input using minibuffer during kbd macro execution.
    With prefix argument, allows you to select what prompt string to use.
    If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((query (lambda () (kbd-macro-query t)))
		 (prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
		 (input (unwind-protect
					(progn
					  (add-hook 'minibuffer-setup-hook query)
					  (read-from-minibuffer prompt))
				  (remove-hook 'minibuffer-setup-hook query))))
	(unless (string= "" input) (insert input))))

; Gets a word as input and makes it a banner with
; a specific font.
(fset 'toilet
   "\C-[x\C-m\C-m\C-@\C-a\C-x\C-k\C-u\C-[!toilet -f broadway_kb '\C-y'\C-m\C-@\C-n\C-n\C-n\C-b\C-[;")

