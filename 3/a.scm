;; 2D array and just check all the indices --> time: O(n) as its 8 index checks for each index (8*n)

;; Scheme doesn't seem like it'd be useful for 2D array stuff.

Could throw everything into a large list and compute the neighbor indices for
every position... it'd be the current index + width + (-1, 0, 1)

Cannot index directly into lists though so it'd have to be a ht or an alist I guess...


more clever ways??

I need to look at each index so that I can at least get the number there.


In numpy i could do this in parallel it feels like where I could have a function which returns
the cell value or 0 depending on presence of symbol neighbor and then accumulate these responses



- Use a padded matrix with non-symbol vals that in edges


Width = 140 chars
Height = 140 lines
- 140**2 seems like a lot of data to hold in an alist...
- with the padding it'll be 142x142


Algo
- Read input into alist with padded linear index keys and vals
- Get neighbor indices
- Check if any neighbor is a symbol
- Add curr val if symbol neighor


Read input func
- incrementing an index and taking a step in the open file EXCEPT when padding
- When we pad we set the index val to . and don't step the file
- We can actually ignore newline chars since they will be padded based on indexing scheme
- When to pad?
- The first and last 142 indices are padded
- Then, we pad 1, take 140 vals, pad again and REPEAT

(string-replace "asd\n123" #\newline #\.)

10 -> 12
5 -> 7

************
*1111111111*
*1111111111*
*1111111111*
*1111111111*
*1111111111*
************

(- (* 12 7) 12)
*************1111111111**1111111111**1111111111**1111111111**1111111111*************


;; Just read in each line of data and append a 1st/last row of .s and add a . to front/back of each row

;; Check whether we're supposed to pad the current index
(define (is-pad-index? row col height width))



  ;; Add padding to the edges of the 2D input
  ;; (let ((padded-height (+ 2 height))
  ;; 	(padded-width (+ 2 width)))
  ;;   (cond
  ;;    ;; First row of padding
  ;;    ((< padded-width index) #t)
  ;;    ;; Last row of padding
  ;;    ((> (- (* padded-height padded-width) padded-width)) #t)

  ;;    ))

(define (read-file-lines file)
  (call-with-input-file file
    (lambda (source)
      (let loop ((lines ()) (line (read-line source)))
	(if (eof-object? line)
	    (reverse lines)
	    (loop (cons line lines) (read-line source)))))))

(define (turn-lines-into-padded-alist lines rows cols)
  (let ((res (range-alist-with-val 0 (+ cols 2) ".")))
    res
    )
  )

(turn-lines-into-padded-alist lines-5 140 140)


(define (range start end)
  (if (>= start end)
      `()
      (cons start (range (+ 1 start) end))))

(define (range-alist start end)
  (map (lambda (x) (cons x x)) (range start end)))

(define (range-alist-with-val start end val)
  (map (lambda (x) (cons x val)) (range start end)))



(range-alist-with-val 3 12 ".")


(define (get-neighbor-indices index width height))

(define (has-symbol-neighbor? index))

(define (foo input-file))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define input-file "~/pg/advent-code-2023/3/input.txt")

;; TESTING
(define lines (read-file-lines input-file))
(define l1 (car lines))
(define l2 (car (cdr lines)))
(define lines-5 (take lines 5))

(length lines-5)
