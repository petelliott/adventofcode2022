(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme cxr)
        (util)
        (srfi 1))

(define (input)
  (define l (read-line))
  (if (eof-object? l)
      '()
      (cons (map (lambda (point) (map string->number (string-split point #\,)))
                 (delete "->" (string-split l #\space)))
            (input))))

(define paths (input))

(define minx1 (fold min 100000000 (map car (fold append '() paths))))
(define maxx (fold max 0 (map car (fold append '() paths))))
(define width1 (+ 1 (- maxx minx1)))
(define height (+ 2 (fold max 0 (map cadr (fold append '() paths)))))
(define width (+ (* 2 height) width1))
;;(define minx (remainder width 2))
(define minx (- minx1 (quotient (* 2 height) 2)))
(write (list minx1 minx))
(newline)


(define grid (list->vector (map (lambda (h) (make-vector width #\.)) (iota height))))

(define (ref x y)
  (if (or (>= x width) (>= y height)
          (< x 0) (< y 0))
      #f
      (vector-ref (vector-ref grid y) x)))

(define (set x y v)
  (if (or (>= x width) (>= y height)
          (< x 0) (< y 0))
      #f
      (begin
        (vector-set! (vector-ref grid y) x v)
        #t)))

(define (print-grid)
  (do ((y 0 (+ y 1))) ((>= y height))
    (do ((x 0 (+ x 1))) ((>= x width))
      (display (ref x y)))
    (newline)))


(define (unitize n)
  (cond
   ((> n 0) 1)
   ((< n 0) -1)
   (else 0)))

(define (stroke to from)
  ;(write (list to from))
  ;(newline)
  (define dx (unitize (- (car to) (car from))))
  (define dy (unitize (- (cadr to) (cadr from))))
  ;(write (list dx dy))
  ;(newline)
  (do ((x (car from) (+ x dx))
       (y (cadr from) (+ y dy)))
      ((and (= x (car to)) (= y (cadr to)))
       (set (- x minx) y #\#))
    ;(write (list (- x minx) y))
    ;(newline)
    (set (- x minx) y #\#))
  to)

(for-each (lambda (path)
            (fold stroke (car path) (cdr path)))
          paths)

(define (sand-sim1 x y)
  (cond
   ((equal? (ref x y) #\o) #f)
   ((not (ref x (+ y 1))) (set x y #\o))
   ((equal? (or (ref x (+ y 1)) #\.) #\.)
    (sand-sim1 x (+ y 1)))
   ((equal? (or (ref (- x 1) (+ y 1)) #\.) #\.)
    (sand-sim1 (- x 1) (+ y 1)))
   ((equal? (or (ref (+ x 1) (+ y 1)) #\.) #\.)
    (sand-sim1 (+ x 1) (+ y 1)))
   (else (set x y #\o))))

(define (sim)
  (if (sand-sim1 (- 500 minx) 0)
      (+ 1 (sim))
      0))

(write (sim))
(newline)
(print-grid)
