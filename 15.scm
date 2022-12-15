(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme cxr)
        (util)
        (srfi 1))

(define (coords s)
  (define xy (string-split s #\,))
  (list (string->number (string-copy (car xy) 2))
        (string->number (string-copy (cadr xy) 3))))

(define (input)
  (define l (read-line))
  (if (eof-object? l)
      '()
      (let ((sp (string-split l #\:)))
        (cons (list (coords (string-copy (car sp) 10))
                    (coords (string-copy (cadr sp) 22)))
              (input)))))

(define data (input))

(define (can-beacon1 s x y)
  (define sx (caar s))
  (define sy (cadar s))
  (define bx (caadr s))
  (define by (cadadr s))
  (or (and (= bx x) (= by y))
      (> (+ (abs (- x sx)) (abs (- y sy)))
         (+ (abs (- bx sx)) (abs (- by sy))))))

(define (can-beacon d x y)
  (or (null? d)
      (and (can-beacon1 (car d) x y)
           (can-beacon (cdr d) x y))))

(define (is-beacon d x y)
  (and (not (null? d))
       (let ((bx (caadr (car d)))
             (by (cadadr (car d))))
         (or (and (= bx x) (= by y))
             (is-beacon (cdr d) x y)))))

(define y 2000000)
;(define y 10)

(define (from start end)
  (if (> start end)
      '()
      (cons start (from (+ start 1) end))))

(define (range s)
  (define sx (caar s))
  (define sy (cadar s))
  (define bx (caadr s))
  (define by (cadadr s))
  (define dist (- (+ (abs (- sx bx)) (abs (- sy by))) (abs (- sy y))))
  (list (- sx dist) (+ sx dist)))

(define (ranges d)
  (fold append '() (map range d)))

(define minx (fold min 1000000000 (ranges data)))
(define maxx (fold max -1000000000 (ranges data)))

(define cb 0)
(do ((x minx (+ x 1)))
    ((> x maxx))
  (if (and (not (is-beacon data x y))
            (not (can-beacon data x y)))
      (set! cb (+ 1 cb))))

;; part1
(write cb)
(newline)

;; part2

(define i 0)
(define (frontier s)
  (display "fronteir ")
  (write i)
  (newline)
  (set! i (+ i 1))
  (define sx (caar s))
  (define sy (cadar s))
  (define bx (caadr s))
  (define by (cadadr s))
  (define dist (+ (abs (- sx bx)) (abs (- sy by))))
  (fold append '()
        (map (lambda (n)
               (define dim (- (+ dist 1) n))
               (list (list (+ sx dim) (+ sy n))
                     (list (+ sx dim) (- sy n))
                     (list (- sx dim) (+ sy n))
                     (list (- sx dim) (- sy n))))
             (iota (+ dist 1)))))

(define (frontiers d)
  (fold append '() (map frontier d)))

(define fs (frontiers data))
(define fs2 (filter (lambda (p)
                      (and (>= 4000000 #;20 (car p) 0)
                           (>= 4000000 #;20 (cadr p) 0)))
                    fs))

(define answer (filter (lambda (p) (can-beacon data (car p) (cadr p))) fs2))
(write (+ (cadar answer) (* 4000000 (caar answer))))
(newline)