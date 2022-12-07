(define-library (util)
  (import (scheme base)
          (scheme read))
  (export string-split
          list-set
          list-head
          string->object)
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

    ))
