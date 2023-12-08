;; Advent of Code 2023 day 3 part 1

(define (read-file-lines file)
  (call-with-input-file file
    (lambda (source)
      (let loop ((lines ()) (line (read-line source)))
	(if (eof-object? line)
	    (reverse lines)
	    (loop (cons line lines) (read-line source)))))))

;; Helper for concise check of boolean list
(define (any-true? x)
  (cond ((null? x) #f)
	((car x) #t)
	(else (any-true? (cdr x)))))

;; Get all the neighbor for an index in a 2D grid
(define (neighbor-indices grid row col)
  (let* ((row-max (length grid))
	 (col-max (string-length (car grid)))
	 (top-row (max 0 (- row 1)))
	 (bottom-row (min (- row-max 1) (+ row 1)))
	 (left-col (max 0 (- col 1)))
	 (right-col (min (- col-max 1) (+ col 1))))
    ;; Filter out the index we are checking (can happen because of the mins/maxs)
    (filter (lambda (index)
	      (not (and (= row (car index)) (= col (cadr index)))))
	    (list
	     (list top-row left-col)
	     (list top-row col)
	     (list top-row right-col)
	     (list row left-col)
	     (list row right-col)
	     (list bottom-row left-col)
	     (list bottom-row col)
	     (list bottom-row right-col)))))

(define (ch-is-symbol ch)
  (not (or (char-alphanumeric? ch) (char=? ch #\.))))

(define (coord-is-symbol? grid coords)
  (let ((row (car coords))
	(col (cadr coords)))
    (ch-is-symbol (string-ref (list-ref grid row) col))))

(define (has-symbol-neighbor? grid row col)
  (let* ((neighbors (neighbor-indices grid row col))
	 (neighbor-symbols (map (lambda (x) (coord-is-symbol? grid x)) neighbors)))
    (any-true? neighbor-symbols)))

;; Returns list of nums and their index start and end
;; ex: s = "1ab22cd333" -> ((1 0 1) (22 3 5) (333 7 10))
(define (extract-nums-and-indices str)
  (define (_extract-nums-and-indices str start end res)
  ;; Defining this add-num-to-res func here to clean up the cond block
  (define (add-num-to-res)
    (let* ((num (string->number (substring str start end)))
	   (new (list num start end)))
      (cons new res)))
    (cond ((= start (string-length str)) res)
	  ;; number at END of string we need to capture
	  ((= end (string-length str)) (add-num-to-res))
	  ;; Increment end index (looking at a number currently)
	  ((char-numeric? (string-ref str end)) (_extract-nums-and-indices str start (+ end 1) res))
	  ;; Just finished scanning number, add to res and continue
	  ((not (= start end)) (_extract-nums-and-indices str (+ end 1) (+ end 1) (add-num-to-res)))
	  ;; Increment start/end and continue looking for numbers
	  (else (_extract-nums-and-indices str (+ start 1) (+ end 1) res))))
  (_extract-nums-and-indices str 0 0 (list)))

;; Provides lists of (row number col-start col-end) for numbers with
;; neighbor symbols (valid) Organized like this ... bc it is...
(define (get-number-row-col-data lines)
  (define (_get-number-row-col-data lines row res)
    (if (null? lines)
	(apply append res)
	(_get-number-row-col-data (cdr lines) (+ row 1)
	      (cons (map (lambda (x) (cons row x))
			 (extract-nums-and-indices (car lines))) res))))
  (_get-number-row-col-data lines 0 (list)))

;; Return boolean for whether any of the indices repr by
;; [row, col-start:col-end] have a symbol neighbor in grid
(define (number-has-symbol-neighbor? grid row col-start col-end)
  (cond ((= col-start col-end) #f)
	((boolean=? #t (has-symbol-neighbor? grid row col-start)) #t)
	(else (number-has-symbol-neighbor? grid row (+ col-start 1) col-end))))

;; Return the number if the number has a symbol neighbor
;; otherwise return 0.
(define (get-num-val grid num-index-data)
  ;; Helper to format the args for the `number-has-symbol-neighbor?` func
  (define (prepare-args num-index-data)
    (let ((row (car num-index-data))
	  (col-start (caddr num-index-data))
	  (col-end (cadddr num-index-data)))
      (list grid row col-start col-end)))
  (if (apply number-has-symbol-neighbor? (prepare-args num-index-data))
      (cadr num-index-data)
      0))

(define (main input-file)
  (let* ((grid (read-file-lines input-file))
	 (num-index-data (get-number-row-col-data grid))
	 (num-vals (map (lambda (x) (get-num-val grid x)) num-index-data)))
    (apply + num-vals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define input-file "~/pg/advent-code-2023/3/input.txt")
(define res (main input-file))
(display res)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTING
;; (define lines (read-file-lines input-file))
;; (define l1 (car lines))
;; (define lines-5 (take lines 5))
;; (define s "1a11abb111baasd8")
;; (extract-nums-and-indices s)
;; (define number-data (get-number-row-col-data lines-5))
;; number-data
