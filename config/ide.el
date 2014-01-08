;; ;; CEDET
;; ;;
;; ;; This file is a mess... How do I fix this?

;; ; Assuming it's installed through ELPA, here's the cheatsheet

;; ; "C-c TAB"  semantic-ia-complete-symbol
;; ; "C-c >"    semantic-complete-analyze-inline
;; ; "C-c j"    semantic-ia-fast-jump
;; ; "C-c d"    semantic-ia-show-doc
;; ; "C-c m"    semantic-ia-show-summary
;; ; "C-c p"    semantic-analyze-proto-impl-toggle
;; ; "C-c g"    semantic-symref-symbol
;; ; "C-c C-r"  semantic-symref

;; ; Load only the minimum
;; ;(semantic-load-enable-minimum-features)
;; ;(semantic-load-enable-code-helpers)
;; ;(semantic-load-enable-gaudy-code-helpers)
;; ;(semantic-load-enable-excessive-code-helpers)
;; ;(semantic-load-enable-semantic-debugging-helpers)

;; ; Override the default value for CEDET working when idle
;; ;(setq-mode-local c-mode semantic-idle-scheduler-idle-time 3)

;; ;; EDE
;; ;  EDE is a project manager system
;; ;  Creates Makefiles or works with already-existing ones
;; ;  'ede-new' creates projects

;; ; This activates it and is necessary for smart-completion
;; (global-ede-mode t)

;; ;; SEMANTIC
;; ; Semantic allows smart code completion/jump/navigation/reference

;; (semantic-mode t)
;; (setq global-semanticdb-minor-mode t)
;; ; Parse code while I'm doing nothing
;; (setq global-semantic-idle-scheduler-mode t)
;; ; Don't show me information while I'm doing nothing
;; (setq global-semantic-idle-summary-mode nil)
;; ;(autoload 'semanticdb "semanticdb" t)

;; (require 'semantic/bovine/gcc)

;; ; names completion and display of tags
;; (autoload 'semantic-ia "semantic-ia" t)
;; ; auto locate system include files
;; (autoload 'semantic-gcc "semantic-gcc" t)

;; ;(semantic-add-system-include "~/3rd-party/boost-1.43.0/include/" 'c++-mode)
;; ;(semantic-add-system-include "~/3rd-party/protobuf-2.3.0/include" 'c++-mode)

;; ; Enables code-completion
;; (setq semantic-load-enable-code-helpers 1)

;; ; Allows/disallows code-folding
;; (setq global-semantic-tag-folding-mode 'nil)

;; ; Shows current function name on the topmost line
;; ;(semantic-stickyfunc-mode 'nil)

;; ;; SRECODE
;; ; SRecode is a template system for code generation
;; (setq global-srecode-minor-mode 'nil)

;; ;; COGRE
;; ; COGRE is a tool to build UNL diagrams

;; ;; NEED TO INSTALL
;;   ;; (local-set-key "\C-c+" 'semantic-tag-folding-show-block)
;;   ;; (local-set-key "\C-c-" 'semantic-tag-folding-fold-block)
;;   ;; (local-set-key "\C-c\C-c+" 'semantic-tag-folding-show-all)
;;   ;; (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all)
;;   ;; (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
;;   ;; (local-set-key "\C-c=" 'semantic-decoration-include-visit)
;;   ;; (local-set-key "\C-ct" 'eassist-switch-h-cpp)
;;   ;; (local-set-key "\C-ce" 'eassist-list-methods)
;; ;(concat essist-header-switches ("hh" "cc"))
;; (semanticdb-enable-gnu-global-databases 'c-mode)
;; (semanticdb-enable-gnu-global-databases 'c++-mode)

;; ;; ctags
;; ;(require 'semanticdb-ectag)
;; ;(semantic-load-enable-primary-exuberent-ctags-support)

;; ;(setq global-semantic-idle-tag-highlight-mode 1)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;(add-to-list 'load-path "~/.emacs.d/kure/ecb-2.40")
;; ;(require 'ecb)
;; ;(require 'ecb-autoloads)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Semantic
;; ;(global-semantic-idle-completions-mode t)
;; ;(global-semantic-decoration-mode t)
;; ;(global-semantic-highlight-func-mode t)
;; ;(global-semantic-show-unmatched-syntax-mode t)

;; ;; CC-mode
;; ;(add-hook 'c-mode-hook '(lambda ()
;; ;		 (setq ac-sources (append '(ac-source-semantic) ac-sources))
;; ;		 (local-set-key (kbd "RET") 'newline-and-indent)
;; ;		 (linum-mode t)
;; ;		 (semantic-mode t)))

;; ;(ac-config-default)

;; ;; GOTTA LEARN HOW TO USE EIDE
;; ;(require 'eide)
;; ;(eide-start)

;; An attempt to make Emacs a nice IDE for C/C++ programming

;; (defun ide-mode-activate ()
;;   "Activates all CEDET and ECB functions."
;;   (interactive)
;;   (global-semanticdb-minor-mode t)
;;   (semantic-mode t)
;;   (global-ede-mode t)

;;   (when (cedet-gnu-global-version-check t)
;;     (semanticdb-enable-gnu-global-databases 'c-mode)
;;     (semanticdb-enable-gnu-global-databases 'c++-mode))

;;   (ecb-activate)
;;   (ecb-change-layout "kure-default")
;;   (my-cedet-hook))

;; (defun ide-mode-deactivate ()
;;   "Deactivates CEDET and ECB."
;;   (interactive)
;;   (global-ede-mode 'nil)
;;   (ecb-deactivate))



