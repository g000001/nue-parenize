;; nue-parenize
;;
;; hook example:
;; (defadvice indent-region (before nue-paren activate)
;;  (nue-paren (region-beginning) (region-end)))

(require 'cl)

(defun nue-parenize (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((parens (count-paren+thesis)))
        (when (apply #'< parens)
          (insert-nue-space) )
        (forward-line) ))))

(defun count-paren+thesis ()
  (interactive)
  (save-excursion
    (let* ((bol (progn (beginning-of-line) (point)))
           (eol (progn (end-of-line) (point)))
           (line (buffer-substring-no-properties bol eol)) )
      (list (- (count ?\( line)
               (count-seq "\\(" line) )
            (- (count ?\) line)
               (count-seq "\\)" line) )))))

(defun count-seq (seq1 seq2)
  (block nil
    (let* ((str seq2)
           (start 0)
           (end (length str))
           (ans 0) )
      (while (< start end)
        (let ((next (search seq1 str :start2 start)))
          (if next
              (progn
                (incf start (1+ next))
                (incf ans) )
              (return ans) )))
      ans )))

(defun insert-nue-space ()
  (save-excursion
    (beginning-of-line)
    (forward-sexp)
    (or (looking-at "[ \n\t]")
        (or (in-string-p)
            (insert " ") ))))
