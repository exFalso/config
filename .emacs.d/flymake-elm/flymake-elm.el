(provide 'flymake-elm)


(defun flymake-elm-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "elm" (list "--make" local-file))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.elm$"
              flymake-elm-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
      (cons '("^\\(.*\\) at (line \\([0-9]+\\), column \\([0-9]+\\)"
              nil 2 3 1)
            (cons '("^\\(.*\\) on line \\([0-9]+\\),"
                    nil 2 nil 1)
                  (cons '("^\\(Could not find .*\\)"
                          nil nil nil 1)
                        flymake-err-line-patterns))))

