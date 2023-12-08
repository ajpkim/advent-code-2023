;; Day 2 Part 1: 12 red cubes, 13 green cubes, and 14 blue cubes

;; More idiomatic version after review... doesn't need to be defined itself really
;;   (fold-left + 0 nums))  ---> accumulate

(define (sum-list nums)
  (if (= 1 (length nums))
      (car nums)
      (+ (car nums)
	 (sum-list (cdr nums)))))

;; More idiomatic way to read in file than day 1 version
(define (read-file-lines file)
  (call-with-input-file file
    (lambda (source)
      (let loop ((lines ()) (line (read-line source)))
	(if (eof-object? line)
	    (reverse lines)
	    (loop (cons line lines) (read-line source)))))))


;; There IS a built in method for this actually `string-trim`
;; (string-trim "  abc" char-set:alphanumeric) -> "abc"
(define (remove-leading-spaces str)
  (let loop ((index 0))
    (cond ((= index (string-length str)) "")
	  ((not (char=? #\space (string-ref str index)))
	   (substring str index (string-length str)))
	  (else (loop (+ 1 index))))))

;; Splits string on a single character delimeter
(define (string-split str delimeter)
  (define (_string-split str delimeter index res)
    (cond ((= index  (string-length str))
	     (reverse (cons str res)))
	  ((string=? delimeter (substring str index (+ 1 index)))
	   (_string-split (substring str (+ 1 index) (string-length str))
			  delimeter
			  0
			  (cons (substring str 0 index) res)))
	  (else (_string-split str delimeter (+ 1 index) res))))
  (_string-split str delimeter 0 ()))

;; Make the game data into nice list of lists where each list is a set of draws
(define (parse-game-data game)
  (let*  ((raw-data (cdr (string-split game ":")))
	  (raw-samples (string-split (car raw-data) ";"))
	  (samples (map (lambda (x) (map remove-leading-spaces (string-split x ","))) raw-samples))
	  (res (map (lambda (x) (lambda (y) (string-split y " ")) x) samples))
	  )
    res
    ))

(define (check-game-sample sample)
  (let ((n (string->number (car (string-split sample " "))))
	(max_n (hash-table/get color-limits-ht (car (cdr (string-split sample " "))) #f)))
    (if (> n max_n) #f #t)))

(define (check-game-set game-set)
  (let ((checks (map check-game-sample game-set)))
    (if (any (lambda (x) (boolean=? #f x)) checks)
	#f #t)))

(define (check-game game)
  (let ((game-sets (parse-game-data game)))
    (if (any (lambda (x) (boolean=? #f x))
	     (map check-game-set game-sets))
	#f #t)))

(define (get-game-id game)
  (let ((x (car (string-split game ":"))))
    (string->number (car (cdr (string-split x " "))))))

;; Returns the game id for the result sum if game is LEGAL
(define (game-value game)
  (let ((game-id (get-game-id game)))
    (if (boolean=? #t (check-game game))
	game-id
	0
	)))

(define (foo input-file)
  (let ((all-games (read-file-lines input-file)))
    (sum-list (map (lambda (game) (game-value game)) all-games))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define input-file "~/pg/advent-code-2023/2/input.txt")
;; Create hash table to store the max number for each color
(define color-limits-ht (make-hash-table equal?))
(hash-table/put! color-limits-ht "red" 12)
(hash-table/put! color-limits-ht "green" 13)
(hash-table/put! color-limits-ht "blue" 14)

;; Run it
(foo input-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;; (define a (read-file-lines input-file))
;; (define g1 (car a))
;; (define g2 (car (cdr a)))
;; (define g3 (car (cdr (cdr a))))
;; (define g3 (car (cdr (cdr a))))
;; (define g1-sets (parse-game-data g1))
;; (define set1 (car g1-sets))
;; (define sample1 (car set1))
