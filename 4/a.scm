;; Advent of Code 2023 day 4 part 1

;; Functions I need:
;; - read lines of input file --> ((card1) (card2)...)
;; - parse-card-string --> ((winning nums) (card nums))
;;   - parse-nums --> (nums in string)
;; - get-card-value --> Return the numeric value of a card string
;; - num-card-matches  -->  return number of winning number matches
;; - main --> sum of all card vals


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

(define (parse-card-string card-str)
  (let* ((num-data (cadr (string-split-on-char card-str #\:)))
	 (num-lists (string-split-on-char num-data #\|)))
    (map parse-nums num-lists)))

;; O(N)... bc we're using lists instead of hash tables
(define (num-in-list? num num-list)
  (if (member num num-list) #t #f))

;; First match worth 1 and all further matches double the score
(define (score-card card-data)
  (define (count-card-matches card-data)
    (let ((winning-nums (car card-data))
	  (nums (cadr card-data)))
      (fold-left (lambda (count n)
		   (+ count (if (num-in-list? n winning-nums) 1 0)))
		 0 nums)))
  (define (score-match-count num-matches)
    (if (< num-matches 2)
	num-matches
	(expt 2 (- num-matches 1))))
  (score-match-count (count-card-matches card-data)))

(define (main input-file)
  (let ((cards (read-file-lines input-file)))
    (apply + (map (lambda (card)
		    (score-card (parse-card-string card)))
		  cards))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define input-file "~/pg/advent-code-2023/4/input.txt")
(define res (main input-file))
(display res)
;; 21558

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define raw-lines (read-file-lines input-file))
;; (define l1 (car raw-lines))
;; (define wn (car card-data))
;; (define n (cadr card-data))
;; (count-card-matches card-data)
;; (define c1 "Card   1: 17 15  5 75 36 13 16 66 92 39 | 13 92 16  5 87 78 15 94 21 48 30 62 70 41  3 39 22 17 77 58 75 52 83 34 24")
;; (parse-nums a)
;; (string-length "")
;; (define a " 1  2 3 a b c 4 5")
;; (parse-nums a)
;; (string-split-on-char "a b   c" #\space)
