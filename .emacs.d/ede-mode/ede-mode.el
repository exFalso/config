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
(define-minor-mode ede-mode
  "EDE mode"
  nil
  nil
  ()
  (if (not ede-mode)
      (font-lock-refresh-defaults)
    (progn
      (font-lock-add-keywords
       nil
       '(("\\({{\\)" 1 'ede-builtin-symbol)
         ("\\({%\\)" 1 'ede-builtin-symbol)
         ("\\(%}\\)" 1 'ede-builtin-symbol)
         ("\\(}}\\)" 1 'ede-builtin-symbol)
         ("\\(}}\\)" 1 'ede-builtin-symbol)
         ("{% [^%]*\\<\\(for\\|endfor\\|endif\\|elif\\|if\\|else\\)\\>[^%]* %}" 1 '(ede-builtin-symbol :weight bold))
         ("{% [^%]*\\<\\(in\\)\\>[^%]* %}" 1 '(ede-builtin-symbol :weight bold))
         ("{% [^%]*\\<\\(for\\)\\> \\([^%]*\\) \\(in\\) \\([^%]*\\) %}" 2 '(ede-variable))
         ("{% [^%]*\\<\\(for\\)\\> \\([^%]*\\) \\(in\\) \\([^%]*\\) %}" 4 '(ede-variable))
         ("{{ \\([^}]*\\) }}" 1 '(ede-variable))
         ("{% +\\<\\(if\\)\\>\\([^%]*\\)%}" 2 'ede-variable)
         ))
      (font-lock-fontify-buffer)
      ))
  )

(add-to-list 'minor-mode-alist '(ede-mode " EDE"))

(provide 'ede-mode)
