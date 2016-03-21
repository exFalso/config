;; (setq debug-on-error t)
(setq initial-scratch-message "")
;; (set-face-attribute 'default nil :height 90)
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-to-load-path
   '("setup-org"
     "setup-mu"
     "popup"
     "php-mode"
     "multi-term"
     "java-improved"
     "inf-groovy"
     "groovy-mode"
     "groovy-electric"
     "grails-mode"
     "git-ediff"
     "genmacros"
     "fuzzy"
     "flymake-escjava2"
     "flymake-elm"
     "flymake-cursor"
     "find-file-in-project"
     "csharp-mode"
     "cppbplate-util"
     "cppbplate"
     "coffee-mode"
     "auto-complete"
     "auto-complete-config"
     "ant-flymake"
     "kotlin-mode"
     "ede-mode"
     "git-find-file"
     )))

(require 'cl)

(autoload 'word-count-mode "word-count" "Minor mode to count words." t nil)
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(set-face-attribute 'region nil :background "#72A0C1")
(set-face-attribute 'region nil :foreground "#FFFFFF")

;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)

;; (require 'coq)

(blink-cursor-mode)
(setq prolog-system 'swi)

(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see `auto-mode-alist'
All elements of this alist are checked, meaning you can enable multiple minor modes for the same regexp."
  )
(defun enable-minor-mode-based-on-extension ()
  "check file name against auto-minor-mode-alist to enable minor modes
the checking happens for all pairs in auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(setq auto-mode-alist
      (append '(("\\.pl$" . prolog-mode)
				("\\.jad$" . java-mode)
				("\\.pde$" . java-mode)
				(".conkerorrc$" . javascript-mode)
                ("\\.sc$" . c-mode)
                ("\\.v$" . coq-mode)
                ("\\.gradle$" . groovy-mode)
                ("\\.hx$" . haxe-mode)
                ("\\.hx.st$" . haxe-mode)
                ("\\.hx.ede$" . haxe-mode)
                ("\\.hs.ede$" . haskell-mode)
                ("\\.module\\.[a-z]+$" . spaghetti-mode)
                ("\\.module$" . spaghetti-mode)
                ("\\.h$" . c++-mode)
                ("\\.cu$" . c++-mode)
                ("\\.cl$" . c-mode)
                ("\\.h\\.[a-z]+$" . c++-mode)
                ("\\.cpp\\.[a-z]+$" . c++-mode)
				("\\.m$" . mercury-mode)
				("\\.scala$" . scala-mode)
				("\\.cu$" . c-mode)
				("\\.ts\\'" . typescript-mode)
				("\\.tsx\\'" . typescript-mode)
                )
              auto-mode-alist))

(setq auto-minor-mode-alist
      (append '(("\\.ede$" . ede-mode))
              auto-minor-mode-alist))

(add-hook 'find-file-hook 'enable-minor-mode-based-on-extension)

(setq c-default-style
      '((c++-mode . "awk") (c-mode . "awk") (java-mode . "java") (groovy-mode . "java") (other . "gnu")))
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(setq x-select-enable-clipboard nil)
(setq inhibit-startup-message t)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil) ;; TABBBB
(require 'find-file-in-project)

(require 'genmacros)
(require 'auto-complete)
(require 'git)
(require 'git-blame)
(require 'kotlin-mode)
(require 'ede-mode)
(require 'git-find-file)

(global-set-key (kbd "C-x f") 'git-find-file)
(global-set-key (kbd "<M-kp-0>") (lambda() (interactive) (compile "make clean && make")))
(global-set-key (kbd "<M-kp-decimal>")(lambda() (interactive) (compile "make clean")))
(global-set-key (kbd "<M-kp-enter>") (lambda() (interactive) (compile "make")))
(global-set-key (kbd "<M-kp-multiply>") (lambda() (interactive)
                                          (compile (concat
                                                    "CURRENT="
                                                    (buffer-file-name)
                                                    " make current"))))
(global-set-key (kbd "<M-kp-add>") (lambda() (interactive) (recompile)))
(global-set-key (kbd "<M-kp-divide>") (lambda() (interactive) (compile "make test")))
(global-set-key [\C-cl] 'org-store-link)
(global-set-key [\C-ca] 'org-agenda)
(global-set-key (kbd "M-#") 'count-number-insert-increase)
(global-set-key (kbd "C-M-#") 'count-number-reset)
(global-set-key (kbd "M-RET") 'flymake-start-syntax-check)
(global-set-key (kbd "C-M-<down>") 'mc/mark-next-like-this)

(defvar autosave-dir
 (substitute-in-file-name "$HOME/.emacs-backup/"))
(setq backup-directory-alist
      `((".*" . , autosave-dir)))
(setq auto-save-file-name-transforms
      `((".*" , autosave-dir t)))


(autoload 'ghc-init "ghc" nil t)
(setq ghc-make-ghc-options '("-XNoMonomorphismRestriction" "-Wall"))
(setq haskell-font-lock-symbols t)
;; (setq haskell-stylish-on-save t)
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (ghc-init)
		(haskell-indentation-mode)
	    ))

(add-hook 'haxe-mode-hook
      (lambda ()
        (setq-local indent-tabs-mode t)
        ))

(add-hook 'haxe-ede-mode-hook
      (lambda ()
        (setq-local indent-tabs-mode t)
        ))

(add-hook 'spaghetti-st-mode-hook
      (lambda ()
        (setq-local indent-tabs-mode t)
        ))

;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-hook 'find-file-hook
          (lambda ()
            (if (string-match "/build/" (file-truename buffer-file-name))
                (progn
                  (read-only-mode)
                  (auto-revert-mode)))))

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-output-view-style (quote (("^pdf$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^pdf$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^pdf$" ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$") "%(o?)evince %dS -paper a4r -s 0 %d") ("^pdf$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)evince %dS -paper a4 %d") ("^pdf$" ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$") "%(o?)evince %dS -paper a5r -s 0 %d") ("^pdf$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)evince %dS -paper a5 %d") ("^pdf$" "^b5paper$" "%(o?)evince %dS -paper b5 %d") ("^pdf$" "^letterpaper$" "%(o?)evince %dS -paper us %d") ("^pdf$" "^legalpaper$" "%(o?)evince %dS -paper legal %d") ("^pdf$" "^executivepaper$" "%(o?)evince %dS -paper 7.25x10.5in %d") ("^pdf$" "." "%(o?)evince %dS %d") ("^pdf$" "." "xpdf -remote %s -raise %o %(outpage)") ("^html?$" "." "netscape %o"))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "Evince") (output-pdf "Evince") (output-html "xdg-open"))))
 '(find-grep-options "-q -I")
 '(haskell-program-name "ghci"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun c++-keybindings ()
  (global-set-key (kbd "M-n") (lambda ()
				(interactive)
			        (flymake-goto-next-error)))
  (global-set-key (kbd "M-p") (lambda ()
				(interactive)
			        (flymake-goto-prev-error))))

(add-hook 'flymake-mode-hook
          (lambda ()
            (require 'flymake-cursor)
            (global-set-key (kbd "M-n")
                            (lambda ()
                              (interactive)
                              (flymake-goto-next-error)
                              (flyc/show-fly-error-at-point-now)))
            (global-set-key (kbd "M-p")
                            (lambda ()
                              (interactive)
                              (flymake-goto-prev-error)
                              (flyc/show-fly-error-at-point-now)))))


(add-hook 'java-mode-hook (lambda ()
			   (require 'flymake-cursor)
			   (flymake-mode)
               (setq flymake-no-changes-timeout 10)
			   (auto-complete-mode)
			   ))

(add-hook 'c++-mode-hook (lambda ()
			   (require 'flymake-cursor)
			   (auto-complete-mode)
			   (c++-keybindings)
			   (setq-local indent-tabs-mode t)
               (set-generic-balance-indent "{\\|(" "}\\|)")
			   ))
(add-hook 'c-mode-hook (lambda () (require 'flymake-cursor)))
(require 'coffee-mode)

(add-hook 'csharp-mode-hook (lambda ()
			   (require 'flymake-cursor)
			   (auto-complete-mode)
			   (c++-keybindings)
			   ))

(add-hook 'typescript-mode-hook (lambda ()
			   (auto-complete-mode)
               (setq-local indent-tabs-mode nil)
			   ))


;; jslint
(when (load "flymake" t)
  (defun flymake-jslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "jslint" (list local-file))))

  (setq flymake-err-line-patterns
	(cons '("^  [[:digit:]]+ \\([[:digit:]]+\\),\\([[:digit:]]+\\): \\(.+\\)$"
		nil 1 2 3)
	      flymake-err-line-patterns))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-jslint-init)))

;; (require 'cppbplate)
;; (require 'csharp-mode)
;; (require 'setup-mu)
;; (require 'setup-org)
;; (require 'flymake-tex)
;; (require 'java-improved)
;; (require 'ant-flymake)
;; (require 'php-mode)

;; groovy
(add-to-list 'load-path "~/.emacs.d/Emacs-Groovy-Mode/")
(require 'groovy-mode)

;; elm

;; (add-to-list 'load-path "~/.emacs.d/elm-mode/")
;; (require 'elm-mode)
;; (require 'flymake-elm)

;; (add-hook 'elm-mode-hook (lambda ()
;;                            ;; (flymake-mode)
;;                            (require 'flymake-cursor)
;;                            (global-set-key (kbd "M-n")
;;                                            (lambda ()
;;                                              (interactive)
;;                                              (flymake-goto-next-error)
;;                                              (flyc/show-fly-error-at-point-now)))
;;                            (global-set-key (kbd "M-p")
;;                                            (lambda ()
;;                                              (interactive)
;;                                              (flymake-goto-prev-error)
;;                                              (flyc/show-fly-error-at-point-now)))
;;                            ))

(require 'generic-x)

(define-generic-mode 'spaghetti-mode
  (cons (cons "/*" "*/") nil)
  '("interface" "struct" "module" "enum" "extern" "import" "const" "extends" "as" "static" "extends")
  '(("\\<\\(?:any\\|int\\|string\\|void\\|float\\|bool\\|[A-Z]\\(\\w\\|_\\)*\\)\\>" . 'font-lock-type-face)
	("\\(@\\(\\w\\|_\\)*\\)" . 'font-lock-builtin-face)
	("\\(?:->\\|=\\)" . 'font-lock-variable-face)
	("\\<\\(\\(\\w\\|_\\)*\\)\\s-*(" 1 'font-lock-function-name-face)
	("\\(module\\)\\s-\\(\\w+\\(\\.\\w+\\)*\\)" 2 'font-lock-constant-face)
	("\\(\\(\\w+\\.\\)+\\)" . 'font-lock-constant-face)
	)
  '("\\.module$")
  '((lambda ()
	  (set (make-local-variable 'indent-line-function) 'spaghetti-indent-line)
	  (local-set-key (kbd "M-;") (lambda ()
								   (interactive)
								   (let (isComment)
									 (progn
									   (beginning-of-line)
									   (setq isComment (search-forward "/* " (line-end-position) t))
									   (if (not isComment)
										   (progn
											 (spaghetti-indent-line)
											 (end-of-line)
											 (if (not (= 0 (current-column)))
												 (insert " "))
											 (insert "/*  */")
											 (forward-char -3)
											 )))))))
	  )
  "spaghetti mode"
)

(defface error-face '((((class color) (min-colors 88) (background light))
                       :foreground "red" :weight bold))
  "Error face"
  :group 'basic-faces
  )

(defun set-generic-balance-indent (beginRegexp endRegexp)
  (set (make-local-variable 'indent-line-function)
       (create-generic-balance-indent-line beginRegexp endRegexp)))

(define-generic-mode 'haxe-mode
  (cons (cons "/*" "*/") nil)
  '("interface" "typedef" "package" "enum" "import" "class" "extends" "as" "static" "implements" "function" "new" "return" "var" "private" "public" "this" "using" "try" "catch" "for" "in" "throw" "if" "else" "switch" "case" "default" "untyped" "inline" "never" "null" "while" "do")
  '(("\\(//.*$\\)" . 'font-lock-comment-face)
	("\\(var\\)\\s-\\(\\(\\w\\|_\\)+\\)" 2 'font-lock-variable-name-face)
    ("[^\\w_][^a-zA-Z0-9]\\(cast\\)[^a-zA-Z0-9]" 1 'error-face)
	("[^\\w_]\\(\\<\\(?:any\\|int\\|string\\|void\\|float\\|bool\\|[A-Z]\\(\\w\\|_\\)*\\)\\>\\)" 1 'font-lock-type-face)
	("\\(@\\(\\w\\|_\\)*\\)" . 'font-lock-builtin-face)
	("\\s-\\<\\(\\(\\w\\|_\\)*\\)\\s-*(" 1 'font-lock-function-name-face)
	("\\(package\\)\\s-\\(\\(\\w\\|_\\)+\\(\\.\\w+\\)*\\)" 2 'font-lock-constant-face)
    ("\\<\\(?:true\\|false\\)\\>" . 'font-lock-constant-face)
	("\\(\\(\\(\\w\\|_\\)+\\.\\)+\\)[A-Z*]" 1 'font-lock-constant-face)
	)
  '("\\.module$")
  '((lambda ()
	  (set (make-local-variable 'indent-line-function) ede-indent-line)
	  (local-set-key (kbd "M-;") (lambda ()
								   (interactive)
								   (let (isComment)
									 (progn
									   (beginning-of-line)
									   (setq isComment (search-forward "/* " (line-end-position) t))
									   (if (not isComment)
										   (progn
											 (funcall ede-indent-line)
											 (end-of-line)
											 (if (not (= 0 (current-column)))
												 (insert " "))
											 (insert "/*  */")
											 (forward-char -3)
											 )))))))
	  )
  "haxe mode"
)

(defface ede-builtin-symbol '((((class color) (min-colors 88) (background light))
                               :foreground "tomato"))
  "EDE builtin symbol face"
  :group 'basic-faces
  )
(defface ede-variable '((((class color) (min-colors 88) (background light))
                         :weight bold))
  "EDE variable face"
  :group 'basic-faces
  )

(define-generic-mode 'haxe-ede-mode
  (list (cons "//" nil) (cons "/*" "*/"))
  '("interface" "typedef" "package" "enum" "import" "class" "extends" "as" "static" "implements" "function" "new" "return" "var" "private" "public" "this" "using" "try" "catch" "throw" "switch" "case" "default" "untyped" "inline" "never" "null" "while" "do")
  '(("\\(//.*$\\)" . 'font-lock-comment-face)
    ("\\({{\\)" 1 'ede-builtin-symbol)
    ("\\({%\\)" 1 'ede-builtin-symbol)
    ("\\(%}\\)" 1 'ede-builtin-symbol)
    ("\\(}}\\)" 1 'ede-builtin-symbol)
    ("\\(}}\\)" 1 'ede-builtin-symbol)
    ("{% [^%]*\\<\\(for\\|endfor\\|endif\\|elseif\\|if\\|else\\)\\>[^%]* %}" 1 '(ede-builtin-symbol :weight bold))
    ("{% [^%]*\\<\\(in\\)\\>[^%]* %}" 1 '(ede-builtin-symbol :weight bold))
    ("{% [^%]*\\<\\(for\\)\\> \\([^%]*\\) \\(in\\) \\([^%]*\\) %}" 2 '(ede-variable))
    ("{% [^%]*\\<\\(for\\)\\> \\([^%]*\\) \\(in\\) \\([^%]*\\) %}" 4 '(ede-variable))
    ("{{ \\([^}]*\\) }}" 1 '(ede-variable))
    ("{% +\\<\\(if\\)\\>\\([^%]*\\)%}" 2 'ede-variable)
	("\\(var\\)\\s-\\(\\(\\w\\|_\\)+\\)" 2 'font-lock-variable-name-face)
	("[^\\w_]\\(\\<\\([A-Z]\\(\\w\\|_\\)*\\)\\>\\)" 1 'font-lock-type-face)
    ("\\<\\(for\\|in\\|if\\|else\\)\\>" 1 'font-lock-keyword-face)
	("\\s-\\<\\(\\(\\w\\|_\\)*\\)\\s-*(" 1 'font-lock-function-name-face)
	("\\(package\\)\\s-\\(\\(\\w\\|_\\)+\\(\\.\\w+\\)*\\)" 2 'font-lock-constant-face)
    ("\\<\\(true\\|false\\)\\>" . 'font-lock-constant-face)
	("\\(\\(\\(\\w\\|_\\)+\\.\\)+\\)[A-Z*]" 1 'font-lock-constant-face)
	)
  '("\\.module$")
  '((lambda ()
	  (set (make-local-variable 'indent-line-function) ede-indent-line)
	  (local-set-key (kbd "M-;") (lambda ()
								   (interactive)
								   (let (isComment)
									 (progn
									   (beginning-of-line)
									   (setq isComment (search-forward "/* " (line-end-position) t))
									   (if (not isComment)
										   (progn
											 (funcall ede-indent-line)
											 (end-of-line)
											 (if (not (= 0 (current-column)))
												 (insert " "))
											 (insert "/*  */")
											 (forward-char -3)
											 )))))))
	  )
  "haxe EDE mode"
)

(define-generic-mode 'spaghetti-st-mode
  (cons (cons "/*" "*/") nil)
  '("interface" "struct" "module" "enum" "extern" "import" "const" "extends" "as" "static" "extends")
  '(("\\(//.*$\\)" . 'font-lock-comment-face)
	("\\(\\$[a-zA-Z0-9().]+[:\\$]\\)" . 'font-lock-variable-name-face)
	("}\\(\\$\\)" 1 'font-lock-variable-name-face)
	("\\<\\(?:any\\|int\\|string\\|void\\|float\\|bool\\|[A-Z]\\(\\w\\|_\\)*\\)\\>" . 'font-lock-type-face)
	("\\(@\\(\\w\\|_\\)*\\)" . 'font-lock-builtin-face)
	("\\(?:->\\|=\\)" . 'font-lock-variable-name-face)
	("\\<\\(\\(\\w\\|_\\)*\\)\\s-*(" 1 'font-lock-function-name-face)
	("\\(module\\)\\s-\\(\\w+\\(\\.\\w+\\)*\\)" 2 'font-lock-constant-face)
	("\\(\\(\\w+\\.\\)+\\)" . 'font-lock-constant-face)
	)
  '("\\.module$")
  '((lambda ()
	  (set (make-local-variable 'indent-line-function) 'spaghetti-indent-line)
	  (local-set-key (kbd "M-;") (lambda ()
								   (interactive)
								   (let (isComment)
									 (progn
									   (beginning-of-line)
									   (setq isComment (search-forward "/* " (line-end-position) t))
									   (if (not isComment)
										   (progn
											 (spaghetti-indent-line)
											 (end-of-line)
											 (if (not (= 0 (current-column)))
												 (insert " "))
											 (insert "/*  */")
											 (forward-char -3)
											 )))))))
	  )
  "spaghetti string templae mode"
)

(defun asd()
  (interactive)
  (font-lock-refresh-defaults))

(defun backwards-nonempty ()
  (interactive)
  (forward-line -1)
  (end-of-line)
  (if (and (not (= (line-number-at-pos) 1))
		   (not (re-search-backward "[^ ]" (line-beginning-position) t)))
	  (backwards-nonempty)
	(end-of-line)))

(defun backwards-proper ()
  (interactive)
  (forward-line -1)
  (beginning-of-line)
  (if (and (not (= (line-number-at-pos) 1))
           (or
            (re-search-forward "^$" (line-end-position) t)
             (re-search-forward "\\(^#\\| *{%\\)" (line-end-position) t)))
	  (backwards-proper)
	(end-of-line)))

(defun balance (beginRegexp endRegexp)
  (let ((foundToken t) (beginCount 0) (endCount 0))
    (save-excursion
      (beginning-of-line)
      (while (re-search-forward beginRegexp (line-end-position) t)
        (setq beginCount (+ beginCount 1)))
      (beginning-of-line)
      (while (re-search-forward endRegexp (line-end-position) t)
        (setq endCount (+ endCount 1)))
      (- beginCount endCount))
    ))


(defun create-generic-balance-indent-line (beginRegexp_ endRegexp_)
  (lexical-let ((beginRegexp beginRegexp_) (endRegexp endRegexp_))
    (lambda()
      (interactive)
      (if (= (line-number-at-pos) 1)
          (indent-line-to 0)
        (let (cur-indent bend bbegin openParen bal1 bal2)
          (save-excursion
            (setq bal1 (balance beginRegexp endRegexp))
            (setq bal1 (if (> bal1 0) 0 bal1))
            (backwards-proper)
            (setq bal2 (balance beginRegexp endRegexp))
            (setq bal2 (if (< bal2 0) 0 bal2))
            (setq cur-indent (+ (current-indentation) (* default-tab-width (+ bal1 bal2))))
            )
          (save-excursion
            (beginning-of-line)
            (if (re-search-forward "^ *#" (line-end-position) t)
                (setq cur-indent 0)))
          (if cur-indent
              (progn
                (let (cur-mark)
                  (progn
                    (setq cur-mark (point-marker))
                    (indent-line-to cur-indent)
                    (if (> cur-mark (point-marker))
                        (goto-char cur-mark))
                    )
                  ))))))))


(defvar ede-indent-line
  (create-generic-balance-indent-line "{\\|(" "}\\|)"))

(defun spaghetti-indent-line ()
  (interactive)
  (if (= (line-number-at-pos) 1)
	  (indent-line-to 0)
	(let (cur-indent bend bbegin openParen)
	  (save-excursion
		(save-excursion
		  (backwards-proper)
		  (end-of-line)
		  (setq openParen (re-search-backward "([^)]*$" (line-beginning-position) t))
		  (if openParen
			  (progn
				(forward-char 1)
				(setq cur-indent (current-column)))))
		(if (not openParen)
			(progn
			  (end-of-line)
			  (setq bend (search-backward "}" (line-beginning-position) t))
			  (if bend
				  (progn
					(forward-char 1)
					(backward-sexp)
					(setq cur-indent (current-indentation)))
				(progn
				  (backwards-proper)
				  (beginning-of-line)
				  (setq bbegin (search-forward "{" (line-end-position) t))
				  (if bbegin
					  (setq cur-indent (+ (current-indentation) default-tab-width))
					(setq cur-indent (current-indentation)))
				  )
				))))
	  (if cur-indent
		  (progn
			(let (cur-mark)
			  (progn
				(setq cur-mark (point-marker))
				(indent-line-to cur-indent)
				(if (> cur-mark (point-marker))
					(goto-char cur-mark))
				)
			))))))


;; TYPESCRIPT
(eval-after-load "typescript"
  '(progn
     ))

;; temp stuff
(add-hook 'typescript-mode-hook (lambda ()
								  (require 'flymake-cursor)
								  (global-set-key (kbd "M-n")
												  (lambda ()
													(interactive)
													(flymake-goto-next-error)
													(flyc/show-fly-error-at-point-now)))
								  (global-set-key (kbd "M-p")
												  (lambda ()
													(interactive)
													(flymake-goto-prev-error)
													(flyc/show-fly-error-at-point-now)))
								  ))


(global-set-key (kbd "C-M-g") (lambda () (interactive)
								(call-interactively 'find-grep-dired)))

(defun count-spaces-or-nil-if-tab (line)

  )

(defun infer-tabs-or-spaces-mode
  (interactive)
  (let ((lineno 0) (maxlineno (line-number-at-pos (end-of-buffer))) (minspace default-tab-width) (tabcount 0))
    (save-excursion
      (while (and (< lineno 100) (<= lineno maxlineno))
        (goto-line lineno)
        (let ((spaceno nil))
          (progn
            (setq spaceno (count-spaces-or-nil-if-tab (thing-at-point 'line t)))
            (if spaceno
                (if (and (not (eq 0 spaceno)) (< spaceno default-tab-width))
                    (setq minspace spaceno))
              (setq tabcount (+ tabcount 1))
            ))
          ))
      (if (> tabcount 0)
          (setq-default indent-tabs-mode t) ;; TABBBB
        (setq default-tab-width minspace))
      )))

;; (require 'haxe-mode)
(require 'git-ediff)
;; (require 'ox-md)

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(add-hook
 'after-init-hook
 (lambda ()
   (progn
     (icy-mode 1)
     (projectile-global-mode)
     )))
