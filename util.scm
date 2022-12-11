(define-library (util)
  (import (scheme base)
          (scheme read)
          (srfi 1))
  (export string-split
          list-set
          list-head
          string->object
          repeat
          maxn)
  (begin

    (define (string-split str char)
      (define l (string-length str))
      (let loop ((i 0))
        (if (>= i l)
            '()
            (do ((j i (+ j 1)))
                ((or (= l j) (equal? (string-ref str j) char))
                 (cons (string-copy str i j) (loop (+ j 1))))))))

    (define (list-set list k obj)
      (cond
       ((= k 0) (cons obj (cdr list)))
       ((null? list) '())
       (else (cons (car list) (list-set (cdr list) (- k 1) obj)))))

    (define (list-head list k)
      (if (= k 0)
          '()
          (cons (car list)
                (list-head (cdr list) (- k 1)))))

    (define (string->object str)
      (call-with-port (open-input-string str)
        (lambda (port)
          (read port))))

    (define (repeat d n)
    (if (= n 0)
        '()
        (cons d (repeat d (- n 1)))))

    (define (1maxn item maxes)
      (cond
       ((null? maxes) '())
       ((> item (car maxes))
        (cons item (reverse (cdr (reverse maxes)))))
       (else (cons (car maxes) (1maxn item (cdr maxes))))))

    (define (maxn lst n)
      (fold 1maxn (make-list n 0) lst))

    ))
