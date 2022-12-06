(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme cxr)
        (util)
        (srfi 1))

(define data (read-line))

(define (distinct l)
  (equal? l (delete-duplicates l)))


(define (is-marker str l i)
  (distinct (string->list str i (+ i l))))

(define (find-marker str len)
  (+ len (do ((i 0 (+ i 1)))
           ((is-marker data len i) i))))

;; part 1

(write (find-marker data 4))
(newline)

;; part 2

(write (find-marker data 14))
(newline)
