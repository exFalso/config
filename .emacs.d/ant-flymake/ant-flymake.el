(push '("^    \\[javac\\] \\(.*\\):\\([0-9]+\\): error: \\(.*\\)$"
        1 2 nil 3) flymake-err-line-patterns)
(setq flymake-err-line-patterns (cons '("SRTool/\\(.*\\):\\([0-9]+\\): error: \\(.*\\)$" 1 2 nil 3) nil))

(defun asdasd ()
  (interactive)
  (progn
    (setq str "    [javac] /data/home/pater/Programming/Java/sr-cw2/SRTool/src/srt/tool/ExprToSmtlibVisitor.java:11: error: class, interface, or enum expected")
    (string-match "\\[javac\\] \\(.*\\):\\([0-9]+\\): error: \\(.*\\)$" str)
    (message (match-string 3 str))))

(provide 'ant-flymake)

