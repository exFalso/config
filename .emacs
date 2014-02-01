;; (setq debug-on-error t)
(setq initial-scratch-message "")
(set-face-attribute 'default nil :height 90)
(add-to-list 'load-path (substitute-in-file-name "$HOME/.emacs.d/"))
(autoload 'word-count-mode "word-count" "Minor mode to count words." t nil)
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(require 'coq)

(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
				("\\.jad$" . java-mode)
				("\\.pde$" . java-mode)
				(".conkerorrc$" . javascript-mode)
                ("\\.sc$" . c-mode)
                ("\\.v$" . coq-mode)
                ("\\.gradle$" . groovy-mode)
                ("\\.hx$" . c++-mode)
				("\\.m$" . mercury-mode))
			      auto-mode-alist))
(setq c-default-style
      '((c++-mode . "awk") (c-mode . "awk") (java-mode . "java") (groovy-mode . "java") (other . "gnu")))
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(setq x-select-enable-clipboard t)
(setq inhibit-startup-message t)
(setq default-tab-width 4)
(setq-default indent-tabs-mode t)

(require 'find-file-in-project)
(global-set-key (kbd "C-x f") 'find-file-in-project)

(require 'genmacros)
(require 'auto-complete)

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

(defvar autosave-dir
 (substitute-in-file-name "$HOME/.emacs-backup/"))
(setq backup-directory-alist
      `((".*" . , autosave-dir)))
(setq auto-save-file-name-transforms
      `((".*" , autosave-dir t)))

(setq ghc-ghc-options '("-XNoMonomorphismRestriction"))

(require 'haskell-mode-autoloads)
;; (load-library "haskell-site-file")
(add-hook 'haskell-mode-hook 
	  (lambda ()
	    (ghc-init)
 	    (flymake-mode)
 	    (haskell-indent-mode)
	    (haskell-style)
        ;; (auto-complete-mode)
        ;; (print "ASDASD")
	    ;; (global-set-key (kbd "M-n") (lambda() (interactive)
		;; 			  (flymake-goto-next-error)
        ;;               (insert "asd")
		;; 			  (ghc-flymake-display-errors)))

	    ;; (global-set-key (kbd "M-p") (lambda() (interactive)
		;; 			  (flymake-goto-prev-error)
		;; 			  (ghc-flymake-display-errors)))
	    ))


(defun haskell-style ()
  "Sets the current buffer to use Haskell Style. Meant to be
added to `haskell-mode-hook'"
  (interactive)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4))


(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))


(autoload 'ghc-init "ghc" nil t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-output-view-style (quote (("^pdf$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^pdf$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^pdf$" ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$") "%(o?)evince %dS -paper a4r -s 0 %d") ("^pdf$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)evince %dS -paper a4 %d") ("^pdf$" ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$") "%(o?)evince %dS -paper a5r -s 0 %d") ("^pdf$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)evince %dS -paper a5 %d") ("^pdf$" "^b5paper$" "%(o?)evince %dS -paper b5 %d") ("^pdf$" "^letterpaper$" "%(o?)evince %dS -paper us %d") ("^pdf$" "^legalpaper$" "%(o?)evince %dS -paper legal %d") ("^pdf$" "^executivepaper$" "%(o?)evince %dS -paper 7.25x10.5in %d") ("^pdf$" "." "%(o?)evince %dS %d") ("^pdf$" "." "xpdf -remote %s -raise %o %(outpage)") ("^html?$" "." "netscape %o"))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "Evince") (output-pdf "Evince") (output-html "xdg-open"))))
 '(agda2-include-dirs (quote ("." "/home/exfalso/Programming/Agda/lib/src"))))
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

;; (add-hook 'flymake-mode-hook
;;           (lambda ()
;;             (require 'flymake-cursor)
;;             (global-set-key (kbd "M-n")
;;                             (lambda ()
;;                               (interactive)
;;                               (flymake-goto-next-error)
;;                               (flyc/show-fly-error-at-point-now)))
;;             (global-set-key (kbd "M-p")
;;                             (lambda () 
;;                               (interactive)
;;                               (flymake-goto-prev-error)
;;                               (flyc/show-fly-error-at-point-now)))))
            

(add-hook 'java-mode-hook (lambda ()
			   (require 'flymake-cursor)
			   ;; (flymake-mode)
			   (auto-complete-mode)
			   ))

(add-hook 'c++-mode-hook (lambda ()
			   (require 'flymake-cursor)
			   (flymake-mode)
			   (auto-complete-mode)
			   (c++-keybindings)
			   ))
(add-hook 'c-mode-hook (lambda () (require 'flymake-cursor)))
(require 'coffee-mode)

(add-hook 'csharp-mode-hook (lambda ()
			   (require 'flymake-cursor)
			   (flymake-mode)
			   (auto-complete-mode)
			   (c++-keybindings)
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

(require 'cppbplate)
(require 'csharp-mode)
(require 'setup-mu)
(require 'setup-org)
(require 'flymake-tex)
(require 'java-improved)
(require 'ant-flymake)
(require 'php-mode)

;; groovy
(add-to-list 'load-path "~/.emacs.d/Emacs-Groovy-Mode/")
(require 'groovy-mode)

;; elm

(add-to-list 'load-path "~/.emacs.d/elm-mode/")
(require 'elm-mode)
(require 'flymake-elm)

(add-hook 'elm-mode-hook (lambda ()
                           ;; (flymake-mode)
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

;; temp stuff

;; (setq flymake-log-level 5)
