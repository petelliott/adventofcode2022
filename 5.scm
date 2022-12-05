(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme cxr)
        (util)
        (srfi 1))

(define (ensure-len l n fill)
  (cond
   ((= n 0) l)
   ((null? l) (cons fill (ensure-len l (- n 1) fill)))
   (else (cons (car l) (ensure-len (cdr l) (- n 1) fill)))))

(define (push l n obj)
  (define newl (ensure-len l (+ n 1) '()))
  (list-set newl n (cons obj (list-ref newl n))))

(define (read-crates)
  (define line (read-line))
  (define len (string-length line))
  (if (equal? (string-ref line 1) #\1)
      '()
      (do ((i 0 (+ i 1))
           (coff 1 (+ coff 4))
           (crates (read-crates)
                   (if (equal? (string-ref line coff) #\space)
                       crates
                       (push crates i (string-ref line coff) ))))
          ((>= coff len) crates))))

(define (read-moves)
  (define line (read-line))
  (if (eof-object? line)
      '()
      (let ((s (string-split line #\space)))
        (cons (list (string->number (cadr s))
                    (string->number (cadddr s))
                    (string->number (caddr (cdddr s))))
              (read-moves)))))

(define crates (read-crates))
(read-line)
(define moves (read-moves))

(define (do-move crates n from to)
  (define stack (list-ref crates (- from 1)))
  (define head (list-head stack n))
  (list-set (list-set crates (- from 1) (list-tail stack n))
            (- to 1) (append (reverse (list-head stack n)) (list-ref crates (- to 1)))))

;; part 1
(define final (fold (lambda (move crates)
                      (apply do-move crates move))
                    crates moves))

(display (list->string (map car final)))
(newline)

;; part 2
(define (do-move2 crates n from to)
  (define stack (list-ref crates (- from 1)))
  (define head (list-head stack n))
  (list-set (list-set crates (- from 1) (list-tail stack n))
            (- to 1) (append (list-head stack n) (list-ref crates (- to 1)))))

(define final2 (fold (lambda (move crates)
                      (apply do-move2 crates move))
                    crates moves))

(display (list->string (map car final2)))
(newline)
