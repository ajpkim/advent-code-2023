(define input-file "/home/ajpkim/pg/advent-code-2023/1/input.txt")

(define (sum nums)
  (if (= 1 (length nums))
      (car nums)
      (+ (car nums)
	 (sum (cdr nums)))))

(define (reverse-string s)
  (list->string (reverse (string->list s))))

(define (read-file-lines input-file)
  (let ((source (open-input-file input-file)))
    (let loop ((lines '())
	       (line (read-line source)))
      (if (eof-object? line)
	  (begin (close-input-port source)
		 (reverse lines))
	  (loop (cons line lines) (read-line source))))))

(define (find-first-num word)
  (let loop ((w word)
	     (ch (string-ref word 0)))
    (if (char-numeric? ch)
	(string->number (string ch))
	(loop (substring w 1 (string-length w))
	      (string-ref w 1)))))

(define (word-to-num word)
  (let ((a (find-first-num word))
	(b (find-first-num (reverse-string word))))
    (+ (* 10 a) b)))

(define (foo input-file)
  (let ((words (read-file-lines input-file)))
    (sum (map word-to-num words))))

(foo input-file)
;; 55123
