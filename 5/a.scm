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

(define (find-min-seed-location seeds maps min-location)
  (if (null? seeds)
      min-location
      (find-min-seed-location
       (cdr seeds)
       maps
       (min min-location (compute-dest (car seeds) maps)))))

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
(define res (main input-file))
(display res)
;; 261668924

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTES
;; Each seed can be converted to a location via the series of 7 range maps we are given.
;; The default for a map is to map 1 -> 1. Each map contains lines indicating range information.


;; The map lines are: destination-range source-range range-length
;; This line from the seed-to-soil map:
;; 1270068015 1235603193 242614277
;; tells us that seeds [1235603193, 1235603193+242614277] --> [1270068015, 1270068015+242614277]


;; In the simple example:
;; 50 98 2
;; 52 50 48

;; seeds [98, 98 + 2] -> [50, 50+2]
;; seeds [50, 50 + 48] -> [52, 52+48]

;; Intervals:
;; 0:49 -> 0:49
;; 50:97 -> 52:99
;; 98:99 -> 50:52


;; MAPS:
;; - seed-to-soil map
;; - soil-to-fertilizer map
;; - fertilizer-to-water map
;; - water-to-light map
;; - light-to-temperature map
;; - temperature-to-humidity map
;; - humidity-to-location

;; Thinking about using series of hashtables to hold map information. Problem here could be storing
;; the entire range in some ht, will run into memory problems since the range is large.

;; Instead, we can match the source number to the specific line in the map which defines its destination
;; and compute it that way.


;; So, we can use functions to output the destination number for each map...
;; This feels inefficient since each function will recompute the mapping function.
;; This is probably fine and now we only need 1 generic compute-dest-num that takes in map info
;; and a source number and outputs the dest number.

;; seed-to-soil map
;; 0 1499266596 13415696
;; 1047526254 72616082 149925679
;; 1197451933 0 72616082
;; 1270068015 1235603193 242614277
;; 13415696 1478217470 21049126
;; 1721726623 2213740950 418202427
;; 2139929050 1721726623 249975615
;; 214214951 369556213 611035599
;; 2389904665 3088515862 345128190
;; 2735032855 2631943377 456572485
;; 3191605340 3433644052 172752250
;; 3364357590 3606396302 421831437
;; 34464822 980591812 179750129
;; 3786189027 1971702238 242038712
;; 825250550 1160341941 75261252
;; 900511802 222541761 147014452


;; FUNCTIONS:
;; 1. parse-input -> seeds list, map lists
;; 2. find-seed-location
;; 3. compute-desitnation args=list of map entries, source number
;; 4. get-range-for-map-entry

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTING

;; (compute-dest 0 (list seed-to-soil-map soil-to-fertilizer-map))
;; (compute-destination 980591814 seed-to-soil-map)
;; 1197451933 ->

;; (define foo (list "1270068015 1235603193 242614277"
;; 		  "13415696 1478217470 21049126"
;; 		  "825250550 1160341941 75261252"
;; 		  "3786189027 1971702238 242038712"
;; 		  "3191605340 3433644052 172752250"
;; 		  "2389904665 3088515862 345128190"
;; 		  "0 1499266596 13415696"
;; 		  "1197451933 0 72616082"
;; 		  "2139929050 1721726623 249975615"
;; 		  "900511802 222541761 147014452"
;; 		  "1047526254 72616082 149925679"
;; 		  "34464822 980591812 179750129"
;; 		  "2735032855 2631943377 456572485"
;; 		  "3364357590 3606396302 421831437"
;; 		  "214214951 369556213 611035599"
;; 		  "1721726623 2213740950 418202427"
;; 		  ))

;; (define seed-to-soil-map (list (list 0 1499266596 13415696)
;; 			       (list 1047526254 72616082 149925679)
;; 			       (list 1197451933 0 72616082)
;; 			       (list 1270068015 1235603193 242614277)
;; 			       (list 13415696 1478217470 21049126)
;; 			       (list 1721726623 2213740950 418202427)
;; 			       (list 2139929050 1721726623 249975615)
;; 			       (list 214214951 369556213 611035599)
;; 			       (list 2389904665 3088515862 345128190)
;; 			       (list 2735032855 2631943377 456572485)
;; 			       (list 3191605340 3433644052 172752250)
;; 			       (list 3364357590 3606396302 421831437)
;; 			       (list 34464822 980591812 179750129)
;; 			       (list 3786189027 1971702238 242038712)
;; 			       (list 825250550 1160341941 75261252)
;; 			       (list 900511802 222541761 147014452)))

;; (define soil-to-fertilizer-map (list (list 226793587 358613369 356867344)
;; 				     (list 0 1838890301 226793587)
;; 				     (list 2741010192 0 358613369)
;; 				     (list 2257843811 715480713 173982825)
;; 				     (list 3099623561 1264222741 3082010)
;; 				     (list 1810570233 2912833547 326150077)
;; 				     (list 4038242924 3815312886 256724372)
;; 				     (list 2431826636 3268919687 279247493)
;; 				     (list 866869902 1671637223 167253078)
;; 				     (list 3102705571 889463538 374759203)
;; 				     (list 1615333950 2646894858 125679350)
;; 				     (list 2136720310 1550513722 121123501)
;; 				     (list 3477464774 2772574208 140259339)
;; 				     (list 1034122980 2065683888 581210970)
;; 				     (list 2711074129 3238983624 29936063)
;; 				     (list 583660931 1267304751 283208971)
;; 				     (list 1741013300 3548167180 69556933)
;; 				     (list 3815312886 4072037258 222930038)))
