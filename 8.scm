(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme cxr)
        (util)
        (srfi 1))

(define (input)
  (define item (read-line))
  (if (eof-object? item)
      '()
      (cons (list->vector (map string->number (map string (string->list item))))
            (input))))

(define data (list->vector (input)))

(define width (vector-length (vector-ref data 0)))
(define height (vector-length data))

(define (in-bound x y)
  (and (>= (- width 1) x 0)
       (>= (- height 1) y 0)))

(define (ref x y)
  (vector-ref (vector-ref data y) x))

;; part 1

(define (visible x y dx dy max)
  (cond
   ((not (in-bound x y)) '())
   ((> (ref x y) max)
    (cons (list x y) (visible (+ x dx) (+ y dy) dx dy (ref x y))))
   (else
    (visible (+ x dx) (+ y dy) dx dy max))))

(define (left)   (fold append '() (map (lambda (row) (visible 0 row 1 0 -1)) (iota height))))
(define (right)  (fold append '() (map (lambda (row) (visible (- width 1) row -1 0 -1)) (iota height))))
(define (top)    (fold append '() (map (lambda (col) (visible col 0 0 1 -1)) (iota width))))
(define (bottom) (fold append '() (map (lambda (col) (visible col (- height 1) 0 -1 -1)) (iota width))))

(write (length (delete-duplicates (append (left) (right) (top) (bottom)))))
(newline)

;; part 2

(define (view-dist x y dx dy h)
  (define x2 (+ x dx))
  (define y2 (+ y dy))
  (cond
   ((not (in-bound x2 y2)) 0)
   ((>= (ref x2 y2) h) 1)
   (else (+ 1 (view-dist x2 y2 dx dy h)))))

(define (senic-score x y)
  (* (view-dist x y 1 0 (ref x y))
     (view-dist x y -1 0 (ref x y))
     (view-dist x y 0 1 (ref x y))
     (view-dist x y 0 -1 (ref x y))))

(define (map-xy proc)
  (fold append '() (map (lambda (x) (map (lambda (y) (proc x y)) (iota height))) (iota width))))

(write (fold max -1 (map-xy senic-score)))
(newline)
