(defun cpp-repeat-string (string n)
  (let ((ret ""))
    (progn
      (while (< 0 n)
        (setq n (- n 1))
        (setq ret (concat string ret))))
    ret))

(defun cpp-namespace-from-filename (&optional filename)
  (if (not filename)
      (setq filename (cpp-filename)))
  (string-match "^\\(\\([a-z]_\\)?[a-z0-9]+\\)" filename)
  (match-string 1 filename))


(defun cpp-filename ()
  (replace-regexp-in-string ".*/" "" buffer-file-name))

(defun cpp-strip-filename-extension (filename)
  (replace-regexp-in-string "\\.[a-zA-Z0-9]+$" "" filename))

(defun cpp-classname-from-filename (&optional filename)
  (let (stripped)
    (progn
      (if (not filename)
          (setq filename (cpp-filename)))
      (setq stripped (cpp-strip-filename-extension filename))
      (replace-regexp-in-string
       "_" ""
       (capitalize
        (replace-regexp-in-string "^\\(\\([a-z]_\\)?[a-z0-9]+\\)" "" stripped)))
      )))

(defun cpp-filename-has-extension (ext &optional filename)
  (progn
    (if (not filename)
        (setq filename (cpp-filename)))
    (string-match (concat "\\." ext "$") filename)
    ))

(defun cpp-is-cpp (&optional filename)
  (cpp-filename-has-extension "cpp" filename))

(defun cpp-is-h (&optional filename)
  (cpp-filename-has-extension "h" filename))

(defun cpp-get-type-forward ()
  (interactive)
  (let (p ep)
    (save-excursion
      (re-search-forward "[^ \n]" nil nil)
      (backward-char 1)
      (setq p (point))
      (setq ep (cpp-type-forward))
      (buffer-substring p ep))))

;; moves cursor after next relevant character
(defun cpp-type-forward ()
  (let (ret (end nil))
    (progn
      (re-search-forward "[^ \n]" nil nil)
      (setq start (- (point) 1))
      (when (eq (char-before (point)) ?:)
        ;; global namespace ::
        (forward-char 1)
        (setq ret (point))
        (re-search-forward "[^ \n]" nil nil))
      (backward-char 1)
      (while (not end)
        (setq ret (cpp-simple-type-forward))
        (if (eq (char-before (point)) ?:)
            (forward-char 1)
          (setq end t)))
      (while (eq (char-before (point)) ?*)
        (setq ret (point))
        (re-search-forward "[^ \n]"))
      (when (eq (char-before (point)) ?&)
        (setq ret (point))
        (re-search-forward "[^ \n]"))
      ret)))


(defun cpp-type-list-forward ()
  (re-search-forward "[^ \n]" nil nil)
  (backward-char 1)
  (when (not (eq (char-after (point)) ?>))
    (let ((end nil))
      (while (not end)
        (cpp-type-forward)
        (when (not (eq (char-before (point)) ?,))
          (setq end t)
          (backward-char 1))
        ))
    ))

;; no resolution operator after, moves cursor after next relevant character
;; returns the end position of the simple type
(defun cpp-simple-type-forward ()
  (let (start p ret)
    (progn
      (re-search-forward "[^ \n]" nil nil)
      (setq start (- (point) 1))
      (re-search-forward "[^a-zA-Z0-9_]" nil nil)
      (when (string-match "$\\(short\\)\\|\\(long\\)\\|\\(volatile\\)\\|\\(const\\)\\|\\(typename\\)"
                          (buffer-substring start (point)))
        (re-search-forward "[a-zA-Z0-9_]" nil nil)
        (setq p (- (point) 1))
        (re-search-forward "[^a-zA-Z0-9_]" nil nil)
        (when (string-match "$\\(short\\)\\|\\(long\\)\\|\\(volatile\\)\\|\\(const\\)\\|\\(typename\\)"
                            (buffer-substring p (point)))
          (re-search-forward "[a-zA-Z0-9_]" nil nil)
          (re-search-forward "[^a-zA-Z0-9_]" nil nil)))
      (backward-char 1)
      (setq ret (point))
      (re-search-forward "[^ \n]" nil nil)
      (if (eq (char-before (point)) ?<)
          (progn
            (cpp-type-list-forward)
            (re-search-forward ">")
            (setq ret (point))
            (re-search-forward "[^ \n]")))
      ret)))

(defun cpp-in-string-literal ()
  (equal 1 (% (count ?\" (buffer-substring (point) (line-beginning-position))) 2)))

(defun cpp-in-comment ()
  (or (cpp-in-ss-comment) (cpp-in-star-comment)))

(defun cpp-in-ss-comment ()
  (string-match
   "//"
   (buffer-substring
    (point)
    (line-beginning-position))))

;; expensive, as we need to take care of "/*"s inside comments as well... i love you c++
(defun cpp-in-star-comment ()
  (interactive)
  (let (start checkstarblock blockstart blockend (blockfound nil))
    (save-excursion
      (setq start (point))
      ;; we have to check from the beginning of the file
      ;; any other method can fail (including starting from the end)
      (goto-char (point-min))
      (setq checkstarblock
            (lambda ()
              (if (re-search-forward "/\\*" nil t)
                  (progn
                    (let ((found t))
                      (while (and found
                                  (or (cpp-in-string-literal)
                                      (cpp-in-ss-comment)))
                        (setq found (re-search-forward "/\\*" nil t)))
                      (setq blockstart (point))
                      (if found
                          (if (re-search-forward "\\*/" nil t)
                              (progn
                                (while (cpp-in-string-literal)
                                  (re-search-forward "\\*/" nil t))
                                (setq blockend (- (point) 1)))
                            ;; /* not closed, so we are in a comment (won't compile tho)
                            (progn
                              (setq blockend (point-max))
                              (goto-char (point-max))
                              t)
                            t)
                        nil)))
                nil)
              ))
      (while (and (< (point) start)
                  (setq blockfound (funcall checkstarblock))))
      (if blockfound
          (and (< start blockend)
               (>= start blockstart))
        nil)
      )))

;; returns a list of blocks starting from the most inner block
;; if the list is empty you are in global scope
;; array/struct initialisation does not count
;; doesn't handle comments well
;; a list element consists of
;; (<type> <block begin pos> <block end pos> [<type specific info>])
;; types:
;; 0: anonymous block
;; 1: extern block
;; 2: namespace block
;; 3: class/struct block
;; 4: if block
;; 5: switch block
;; 6: function block
;; 7: statement expression block
;; 8: for block
;; 9: while block
;; 10: do block
(defun cpp-current-block-list ()
  (let ((scopelist nil) blockbegin blockend cursorp continue p thing)
    (save-excursion
      (setq continue t)
      (setq cursorp (point))
      (goto-char (point-min))
      (while (and continue
                  (re-search-forward "{" nil t)
                  (not (cpp-in-string-literal))
                  (not (cpp-in-comment)))
        (backward-char 1)
        (setq blockbegin (point))
        ;; check if we passed cursorp

        (if (>= (point) cursorp)
            (setq continue nil)
          (progn
            ;; check if cursorp is in this block
            (forward-sexp)
            (when (> (point) cursorp)
              (setq blockend (point))
              (goto-char blockbegin)
              ;; what kind of {} is this?
              (re-search-backward "[^ \n]" nil nil)
              (cond ((or (eq (char-after (point)) ?\;)
                         (eq (char-after (point)) ?\{)
                         (eq (char-after (point)) ?\})
                         (eq (char-after (point)) ?\:))
                     (setq scopelist (cons (list 0 blockbegin blockend) scopelist)))
                    ((eq (char-after (point)) ?\()
                     (setq scopelist (cons (list 7 blockbegin blockend) scopelist)))
                    ((eq (char-after (point)) ?\")
                     (setq scopelist (cons (list 1 blockbegin blockend) scopelist)))
                    ((eq (char-after (point)) ?\=))
                    ((eq (char-after (point)) ?\))
                     ;; if,for,while,function
                     (forward-char 1)
                     (backward-sexp)
                     (re-search-backward "[^ \n]" nil nil)
                     (forward-char 1)
                     (setq p (point))
                     (backward-sexp)
                     (setq thing (buffer-substring (point) p))
                     (if (string-match
                          "^\\(\\(if\\)\\|\\(for\\)\\|\\(while\\)\\|\\(switch\\)\\)$"
                          thing)
                         (cond ((match-string 2 thing) ;if	
                                (setq scopelist (cons (list 4 blockbegin blockend 0)
                                                      scopelist)))
                               ((match-string 3 thing) ;for
                                (setq scopelist (cons (list 8 blockbegin blockend)
                                                      scopelist)))
                               ((match-string 4 thing) ;while
                                (setq scopelist (cons (list 9 blockbegin blockend)
                                                      scopelist)))
                               ((match-string 5 thing) ;switch
                                (setq scopelist (cons (list 5 blockbegin blockend)
                                                      scopelist))))
                       ;; function
                       (setq scopelist (cons (list 6 blockbegin blockend) scopelist)))
                       
                     )
                    ((eq (char-after (point)) ?\>)
                     ;; class/struct extending a template class
                     (setq scopelist (cons (list 3 blockbegin blockend) scopelist)))
                    (t
                     ;; namespace, class, struct, do, else
                     (forward-char 1)
                     (setq p (point))
                     (re-search-backward "[^a-zA-Z0-9_]" nil nil)
                     (forward-char 1)
                     (setq thing (buffer-substring (point) p)) ; <thing> {...}
                     (if (string-match
                          "^\\(\\(namespace\\)\\|\\(class\\)\\|\\(struct\\)\\|\\(do\\)\\|\\(else\\)\\)$"
                                  thing)
                         (cond ((match-string 2 thing) ; unnamed namespace
                                (setq scopelist (cons (list 2 blockbegin blockend) scopelist)))
                               ((or (match-string 3 thing) (match-string 4 thing)) ; unnamed class/struct
                                (setq scopelist (cons (list 3 blockbegin blockend) scopelist)))
                               ((match-string 5 thing) ; do
                                (setq scopelist (cons (list 10 blockbegin blockend) scopelist)))
                               ((match-string 6 thing) ; else
                                (setq scopelist (cons (list 4 blockbegin blockend 1) scopelist))))
                       (progn
                         ;; named namespace/class/struct
                         (re-search-backward "[^ \n]" nil nil)
                         (setq p (+ (point) 1))
                         (re-search-backward "[^a-zA-Z0-9_]" nil nil)
                         (if (string-match "^namespace$"
                                           (buffer-substring (+ (point) 1) p))
                             ;; named namespace
                             (setq scopelist (cons (list 2 blockbegin blockend) scopelist))
                           ;; can only be a named class/struct
                           (setq scopelist (cons (list 3 blockbegin blockend) scopelist))))))
                    ))))
        (goto-char blockbegin)
        (forward-char 1))
      scopelist)))

(provide 'cppbplate-util)
