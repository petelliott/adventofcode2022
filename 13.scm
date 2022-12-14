(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme cxr)
        (util)
        (srfi 1))


(define (13read-inner n depth)
  (define c (read-char))
  (if (eof-object? c)
      c
      (case c
        ((#\space #\newline) (13read-inner n depth))
        ((#\[)
         (if (= depth 0)
             (13read-inner #f (+ depth 1))
             (cons (13read-inner #f (+ depth 1)) (13read-inner #f (+ depth 1)))))
        ((#\])
         (if n (list n) '()))
        ((#\,)
         (if n (cons n (13read-inner #f depth)) (13read-inner #f depth)))
        (else
         (13read-inner (+ (* 10 (or n 0)) (- (char->integer c) (char->integer #\0)))
                 depth)))))

(define (13read)
  (13read-inner #f 0))

(define (input)
  (define a (13read))
  (if (eof-object? a)
      '()
      (cons (cons a (13read)) (input))))

(define data (input))

;;; part 1

(define (list-val obj)
  (if (integer? obj)
      (list obj)
      obj))

(define (part1 left right)
  (cond
   ((and (integer? left) (integer? right))
    (cond
     ((= left right) #f)
     ((< left right) 'right)
     (else 'wrong)))
   ((or (integer? left) (integer? right))
    (part1 (list-val left) (list-val right)))
   ((and (null? left) (null? right)) #f)
   ((null? left) 'right)
   ((null? right) 'wrong)
   (else (or (part1 (car left) (car right))
             (part1 (cdr left) (cdr right))))))

(write (fold + 0 (map (lambda (p i)
                        (define r (part1 (car p) (cdr p)))
                        (if (or (eq? r 'right) (not r))
                            (+ i 1) 0))
                      data (iota (length data)))))
(newline)


;; part 2

(define (unpair l)
  (if (null? l)
      '()
      (cons (caar l) (cons (cdar l) (unpair (cdr l))))))

(define sorted (sort (append '(((2)) ((6))) (unpair data)) (lambda (l r) (eq? (part1 l r) 'right))))

(define (part2 l n i)
  (cond
   ((null? l) n)
   ((or (equal? (car l) '((2)))
        (equal? (car l) '((6))))
    (part2 (cdr l) (* n i) (+ i 1)))
   (else
    (part2 (cdr l) n (+ i 1)))))

(write (part2 sorted 1 1))
(newline)
