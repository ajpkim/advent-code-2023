;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic funcs
(define (read-file-lines file)
  (define (get-lines source lines)
    (let ((line (read-line source)))
      (if (eof-object? line)
	  (reverse lines)
	  (get-lines source (cons line lines)))))
  (call-with-input-file file
    (lambda (source) (get-lines source (list)))))

;; Extract all the numbers from a string
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

(define (string-ends-with? str x)
  (let ((str-len (string-length str))
	(x-len (string-length x)))
    (if (> x-len str-len)
	#f
	(string=? x (substring str (- str-len x-len))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem specific funcs

;; Return whether the start number is defined in this interval
;; given a start number and a map interval definition,
(define (num-in-map-interval-range num map-interval)
  (let* ((src-start (cadr map-interval))
	 (src-end (+ src-start (caddr map-interval))))
    (and (>= num src-start) (< num src-end))))

;; Get the destination number associated with the src number
;; and this map interval definition.
;; (Add the distance from num and source start to dest start)
(define (apply-map-interval num map-interval)
  (let* ((src-start (cadr map-interval))
	 (dest-start (car map-interval))
	 (offset (- num src-start)))
    (+ dest-start offset)))

;; Recursively compute the destination number given a start num and
;; set of map-interval definitions.
(define (compute-map-dest num map-intervals)
  (cond ((null? map-intervals) num)
	((num-in-map-interval-range num (car map-intervals))
	 (apply-map-interval num (car map-intervals)))
	(else (compute-map-dest num (cdr map-intervals)))))

;; Compute the final destination number for number and a series of maps
(define (compute-dest num maps)
  (if (null? maps)
      num
      (compute-dest
       (compute-map-dest num (car maps))
       (cdr maps))))

;; Modified from part 1 so we can work with the seed range data
;; without precomputing all the seeds
(define (find-min-seed-location seed-ranges maps min-location)
  ;; Increment the current seed and decrement the remaining range
  (define (update-curr-seed-range)
    (let ((next-seed (+ (car seed-ranges) 1))
	  (remaining (- (cadr seed-ranges) 1)))
      (cons next-seed (cons remaining (cdr (cdr seed-ranges))))))

  (cond ((null? seed-ranges) min-location)
	;; we've exhausted the current range
	((= 0 (cadr seed-ranges))
	 (find-min-seed-location (cdr (cdr seed-ranges)) maps min-location))
	(else (find-min-seed-location
	       (update-curr-seed-range)
	       maps
	       (min min-location (compute-dest (car seed-ranges) maps))))))

;; Recursive range hits recursion limit quickly
;; (define (range start end)
;;   (if (= start end)
;;       (list)
;;       (cons start (range (+ start 1) end))))

;; ;; Iterative hits memory limits
;; (define (range-iter start end)
;;   (let loop ((curr start)
;; 	     (res ()))
;;     (if (>= curr end)
;; 	(reverse res)
;; 	(loop (+ curr 1) (cons curr res)))))

;; (define (parse-seeds seed-ranges)
;;   (define (expand-seed-ranges seed-ranges seeds)
;;     (if (null? seed-ranges)
;; 	(apply append seeds)  ;; flatten the sub lists into single seed list
;; 	(expand-seed-ranges
;; 	 (cdr (cdr seed-ranges))
;; 	 (cons (range (car seed-ranges) (+ (car seed-ranges) (cadr seed-ranges))) seeds))))
;;   (expand-seed-ranges seed-ranges ()))

;; Create data object that has seed and all map information:
;; ((seed1 seed2 ... ) (((map1 interval 1) (map 1 interval 2) ... ) (map2 intervals) ... ))
(define (parse-input input-file)
  (define (parse-input-lines lines data)
    (define (add-interval-to-data line)
      (cons (cons (parse-nums line) (car data)) (cdr data)))
    (cond ((null? lines) (reverse data))  ;; Done processing all lines
	  ;; Initialize the seed nums
	  ((null? data) (parse-input-lines (cdr lines) (cons (parse-nums (car lines)) ())))
	  ((string=? "" (car lines)) (parse-input-lines (cdr lines) data))
	  ;; Prepare to add new map to data
	  ((string-ends-with? (car lines) "map:") (parse-input-lines (cdr lines) (cons () data)))
	  ;; Collect all the intervals for some map
	  ((char-numeric? (string-ref (car lines) 0))
	   (parse-input-lines (cdr lines) (add-interval-to-data (car lines))))
	  ;; Skip blank or extraneous lines
	  (else (parse-input-lines (cdr lines) data))))
  (parse-input-lines (read-file-lines input-file) ()))

(define (main input-file)
  (let* ((parsed-input (parse-input input-file))
	 (seeds (car parsed-input))
	 (maps (cdr parsed-input)))
    (find-min-seed-location seeds maps 1e+1000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define input-file "~/pg/advent-code-2023/5/input.txt")
(define input-test "~/pg/advent-code-2023/5/test-input/input.txt")

;; Running into performance issues with the large number of seeds in part 2.
(display (main input-test))
;; (define res (main input-file))
;; (display res)




;; (define seeds (car pi))
;; (define a (car seeds))
;; (define b (+ a (car seeds)))

;; (range-iter a b)

;; (define pi (parse-input input-file))

;; (parse-seeds (car pi))
