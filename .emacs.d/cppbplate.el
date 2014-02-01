(require 'cppbplate-util)

(defun cpp-test ()
  (interactive)
  (message (cpp-get-type-forward)))


(defun cpp-header ()
  (interactive)
  (if (equal (buffer-size) 0)
      (let (resumepoint filename filenamestr))))

(defun cpp-print-block-list ()
  (interactive)
  (setq blocklist (cpp-current-block-list))
  (let ((n 0) elem type (toprint ""))
    (progn
      (while (setq elem (nth n blocklist))
        (setq n (+ 1 n))
        (setq type (nth 0 elem))
        (setq toprint
              (concat toprint
                      (cond ((eq type 0) ":anonymous")
                            ((eq type 1) ":extern")
                            ((eq type 2) ":namespace")
                            ((eq type 3) ":class")
                            ((eq type 4) ":if")
                            ((eq type 5) ":switch")
                            ((eq type 6) ":function")
                            ((eq type 7) ":statement")
                            ((eq type 8) ":for")
                            ((eq type 9) ":while")
                            ((eq type 10) ":do")
                            )))
        (setq toprint (concat toprint
                              "("
                              (number-to-string (nth 1 elem)) ", "
                              (number-to-string (nth 2 elem))
                              ")")))
      (message toprint))))
(provide 'cppbplate)


