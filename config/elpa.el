;; The lovely Emacs package archive.
;;
;; Finally, instead of copying all these .el
;; files manually...

; Package Manager is Only Available over Emacs 24
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)

  ; Adding several repositories to the packages
  (add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/")  t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("elpa"      . "http://tromey.com/elpa/")             t)
  (add-to-list 'package-archives '("org"       . "http://orgmode.org/elpa/")            t))

; Small fix
(defun package-update-load-path ()
  "Update the load path for newly installed packages."
  (interactive)
  (let ((package-dir (expand-file-name package-user-dir)))
	(mapc (lambda (pkg)
			(let ((stem (symbol-name (car pkg)))
				  (version "")
				  (first t)
				  path)
			  (mapc (lambda (num)
					  (if first
						  (setq first nil)
						(setq version (format "%s." version)))
					  (setq version (format "%s%s" version num)))
					(aref (cdr pkg) 0))
			  (setq path (format "%s/%s-%s" package-dir stem version))
			  (add-to-list 'load-path path)))
		  package-alist)))

