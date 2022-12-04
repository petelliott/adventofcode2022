(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme char)
        (scheme cxr)
        (srfi 1))

(define (halve str)
  (define l (string-length str))
  (define h (/ l 2))
  (cons (string->list (string-copy str 0 h))
        (string->list (string-copy str h))))

(define (input)
  (define line (read-line))
  (if (eof-object? line)
      '()
      (cons line
            (input))))

(define (priority char)
  (if (char>=? char #\a)
      (- (char->integer char) (char->integer #\a) -1)
      (- (char->integer char) (char->integer #\A) -27)))

(define (1intersect l1 l2)
  (cond
   ((or (null? l1) (null? l2)) #f)
   ((member (car l1) l2) => car)
   (else (1intersect (cdr l1) l2))))

(define data (input))
(define split-data (map halve data))

(write (fold + 0 (map (lambda (rs) (priority (1intersect (car rs) (cdr rs)))) split-data)))
(newline)

;; part 2

(define (3intersect l1 l2 l3)
  (cond
   ((null? l1) #f)
   ((and (member (car l1) l2) (member (car l1) l3)) => car)
   (else (3intersect (cdr l1) l2 l3))))

(define (firstn l n)
  (if (= 0 n)
      '()
      (cons (car l) (firstn (cdr l) (- n 1)))))

(define (group l n)
  (if (null? l)
      '()
      (cons (firstn l n)
            (group (list-tail l n) n))))

(write (fold + 0 (map (lambda (g) (priority (3intersect (car g) (cadr g) (caddr g)))) (group (map string->list data) 3))))
(newline)
