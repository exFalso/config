(provide 'flymake-escjava2)

(defun flymake-escjava2-init ()
  ;; (let* ((temp-file (flymake-init-create-temp-buffer-copy
  ;;                    'flymake-create-temp-inplace))
  ;;        (local-file (file-relative-name
  ;;                     temp-file
  ;;                     (file-name-directory buffer-file-name))))
    (list "escjava2" (list "*.java" "-NoWarn IndexTooBig -NoWarn IndexNegative -NoWarn ArrayStore
-NoWarn Null -NoWarn NegSize -NoWarn Modifies")));buffer-file-name)))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.java$"
              flymake-escjava2-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

