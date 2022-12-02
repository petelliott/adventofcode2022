(import (scheme base)
        (scheme write)
        (scheme read)
        (srfi 1))

(define scoremap
  `(((A X) . ,(+ 1 3))
    ((A Y) . ,(+ 2 6))
    ((A Z) . ,(+ 3 0))
    ((B X) . ,(+ 1 0))
    ((B Y) . ,(+ 2 3))
    ((B Z) . ,(+ 3 6))
    ((C X) . ,(+ 1 6))
    ((C Y) . ,(+ 2 0))
    ((C Z) . ,(+ 3 3))))

(define (score data scoremap)
  (fold + 0 (map (lambda (key) (cdr (assoc key scoremap))) data)))

(define (input)
  (define a (read))
  (if (eof-object? a)
      '()
      (cons (list a (read))
            (input))))

(define data (input))

(write (score data scoremap))
(newline)

;; part 2

(define scoremap2
  `(((A X) . ,(+ 3 0))
    ((A Y) . ,(+ 1 3))
    ((A Z) . ,(+ 2 6))
    ((B X) . ,(+ 1 0))
    ((B Y) . ,(+ 2 3))
    ((B Z) . ,(+ 3 6))
    ((C X) . ,(+ 2 0))
    ((C Y) . ,(+ 3 3))
    ((C Z) . ,(+ 1 6))))

(write (score data scoremap2))
(newline)
