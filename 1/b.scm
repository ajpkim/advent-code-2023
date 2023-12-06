(define input-file "/home/ajpkim/pg/advent-code-2023/1/input.txt")

(define (sum-list nums)
  (if (= 1 (length nums))
      (car nums)
      (+ (car nums)
	 (sum-list (cdr nums)))))

(define (read-file-lines input-file)
  (let ((source (open-input-file input-file)))
    (let loop ((lines '())
	       (line (read-line source)))
      (if (eof-object? line)
	  (begin (close-input-port source)
		 (reverse lines))
	  (loop (cons line lines) (read-line source))))))

;; Get the numeric value that ends at the given index that could be a
;; numeric character e.g. '2' or a substring representing a number e.g. 'four'.
;; Given word 'abc4' and the index 3 --> 4
;; Given word 'abc4nine' and the index 7 --> 9
(define (get-num-at-index word index)
  (let ((char (string-ref word index))
	(a (substring word (max 0 (- index 4)) (+ 1 index)))
	(b (substring word (max 0 (- index 3)) (+ 1 index)))
	(c (substring word (max 0 (- index 2)) (+ 1 index))))
    (cond
     ((char-numeric? char) (string->number (string char)))
     ((string=? c "one") 1)
     ((string=? c "two") 2)
     ((string=? a "three") 3)
     ((string=? b "four") 4)
     ((string=? b "five") 5)
     ((string=? c "six") 6)
     ((string=? a "seven") 7)
     ((string=? a "eight") 8)
     ((string=? b "nine") 9)
     (else ()))))

;; Parse all the numbers represented in the word.
(define (get-word-nums word)
  (let loop ((index 0)
	     (nums ()))
    (if (= index (string-length word))
	(filter (lambda (x) (not (null? x))) (reverse nums))
	(loop (+ 1 index)
	      (cons (get-num-at-index word index) nums)))))

(define (word-to-num word)
  (let* ((nums (get-word-nums word))
	(a (car nums))
	(b (car (reverse nums))))
    (+ (* 10 a) b)))

(define (foo input-file)
  (let ((words (read-file-lines input-file)))
    (sum-list (map word-to-num words))))

(foo input-file)
;; 55260
