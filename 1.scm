(import (scheme base)
        (scheme write)
        (srfi 1))
;        (srfi 132))

(define (input-elf)
  (define l (read-line))
  (if (or (eof-object? l) (equal? l ""))
      '()
      (cons (string->number l)
            (input-elf))))

(define (input)
  (define elf (input-elf))
  (if (null? elf)
      '()
      (cons elf (input))))

(define data (input))

(define calories (map (lambda (l) (fold + 0 l)) data))

(write (fold max 0 calories))
(newline)

;; part 2

(define (1maxn item maxes)
  (cond
   ((null? maxes) '())
   ((> item (car maxes))
    (cons item (reverse (cdr (reverse maxes)))))
   (else (cons (car maxes) (1maxn item (cdr maxes))))))

(define (maxn lst n)
  (fold 1maxn (make-list n 0) lst))

(write (fold + 0 (maxn calories 3)))
(newline)
