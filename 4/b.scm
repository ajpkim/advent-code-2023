;; Create an alist that tracks the number of matches for each card
;; so that when we process a card we can check in constant time the number of
;; copies it creates, we then add these copies to our stack or w/e collection.

;; We can initialize our stack with all of our original cards. and increment some counter
;; everytime we pop from the stack which will then accumulate the total number of cards + copies won.

;; FUNCTIONS:
;; - parse-card-string
;; - num-card-matches?
;; - process-card (this function adds to the stack based on card value incl card number)
;; - add-copies-to-card-list args=card-num num-matches (recursive decrement num-matches and use the card-data-alist)

;; PSUEDO
;; - Init card-data-alist with original cards (1 . (1 (winning nums) (card nums)) ... )
;; - Init card-list with all original cards
;; - If there is no card-list return card-count
;; - Otherwise process the front card and increment card-count
;; - Get num card matches (Check if current card has been processed before)
;; - Add the new card copies to card-list using
;; - Continue

(define (read-file-lines file)
  (define (get-lines source lines)
    (let ((line (read-line source)))
      (if (eof-object? line)
	  (reverse lines)
	  (get-lines source (cons line lines)))))
  (call-with-input-file file
    (lambda (source) (get-lines source (list)))))

;; Improved version from day 1 (no subtrings operations and use start/end pointers instead)
(define (string-split-on-char str char-delim)
  (define (_string-split str char-delim start end res)
    (define (add-str-to-res)
      (cons (substring str start end) res))
    (cond ((= start (string-length str)) (reverse res))
	  ((= end (string-length str)) (reverse (add-str-to-res)))
	  ((char=? char-delim (string-ref str end))
	   (_string-split str char-delim (+ end 1) (+ end 1) (add-str-to-res)))
	  (else (_string-split str char-delim start (+ end 1) res))))
  (_string-split str char-delim 0 0 `()))

(define (parse-nums str)
  (define (_parse-nums str start end nums)
    (define (add-num-to-res)
      (cons (string->number (substring str start end)) nums))
    (cond ((= start (string-length str)) (reverse nums))
	  ((= end (string-length str)) (reverse (add-num-to-res)))
	  ((char-numeric? (string-ref str end)) (_parse-nums str start (+ end 1) nums))
	  ((not (= start end)) (_parse-nums str end end (add-num-to-res)))
	  (else (_parse-nums str (+ start 1) (+ end 1) nums))))
  (_parse-nums str 0 0 `()))

;; Return (card-number (winning-nums) (card-nums)) from raw card string.
(define (parse-card-string card-str)
  (let* ((card-data (string-split-on-char card-str #\:))
	 (card-num (string->number (car (reverse (string-split-on-char (car card-data) #\space)))))
	 (num-data (cadr card-data))
	 (num-lists (string-split-on-char num-data #\|)))
    (cons card-num (map parse-nums num-lists))))

(define (num-card-matches card-data)
  (define (num-in-list? num num-list)
    (if (member num num-list) #t #f))
  (let ((winning-nums (cadr card-data))
	(nums (caddr card-data)))
    (fold-left (lambda (count n)
		 (+ count (if (num-in-list? n winning-nums) 1 0)))
	       0 nums)))


(define (process-card-list card-list og-card-list count)

  (define (add-copies-to-card-list card-num num-matches card-list)
    (let ((card-copy (list-ref og-card-list (- (+ card-num num-matches) 1))))
      (if (= 0 num-matches)
	  card-list
	  (add-copies-to-card-list card-num (- num-matches 1) (cons card-copy card-list)))))

  (define (process-card card-data card-list)
    (let ((card-num (car card-data))
	  (num-matches (num-card-matches card-data)))
    (add-copies-to-card-list card-num num-matches card-list)))

  (if (null? card-list)
      count
      (process-card-list (process-card (car card-list) (cdr card-list))
			 og-card-list
			 (+ count 1))))

(define (main input-file)
  (let* ((card-strings (read-file-lines input-file))
	 (og-card-list (map parse-card-string card-strings))
	 (card-list (map parse-card-string card-strings)))
    (process-card-list card-list og-card-list 0)))

(main input-file)  ;; Stalls out
(main test-file)

;; Trying to find input size that fails...
(define input-file "~/pg/advent-code-2023/4/input.txt")
(define test-file "~/pg/advent-code-2023/4/input-test.txt")
(define test-file "~/pg/advent-code-2023/4/input-test-5.txt")
(define test-file "~/pg/advent-code-2023/4/input-test-20.txt") ;; works
(define test-file "~/pg/advent-code-2023/4/input-quarter.txt")
(define test-file "~/pg/advent-code-2023/4/input-half.txt")
(define test-file "~/pg/advent-code-2023/4/input-quarter.txt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c1 "Card   1: 17 15  5 75 36 13 16 66 92 39 | 13 92 16  5 87 78 15 94 21 48 30 62 70 41  3 39 22 17 77 58 75 52 83 34 24")
(define pc1 (parse-card-string c1))

pc1
(num-card-matches pc1)

(define card-strings (read-file-lines input-file))
(define og-card-list (map parse-card-string card-strings))
(define card-list (map parse-card-string card-strings))
