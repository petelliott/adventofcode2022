(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme char)
        (scheme cxr)
        (util)
        (srfi 1))

(define (input)
  (define l (read-line))
  (if (eof-object? l)
      '()
      (cons (map (lambda (s)
                   (map string->number (string-split s #\-)))
                 (string-split l #\,))
            (input))))

(define data (input))

(define (bool->num a)
  (if a 1 0))

(define (bool-map-sum l f)
  (fold + 0 (map bool->num
                 (map (lambda (a)
                        (apply f a)) l))))

;; part 1

(define (contains outer inner)
  (and (<= (car outer) (car inner))
       (>= (cadr outer) (cadr inner))))

(define (either-contains a b)
  (or (contains a b) (contains b a)))

;(write (fold + 0 (map bool->num (map (lambda (a) (apply either-contains a)) data))))
(write (bool-map-sum data either-contains))
(newline)

;; part 2

(define (overlap a b)
  (or (<= (car a) (car b) (cadr a))
      (<= (car a) (cadr b) (cadr a))
      (<= (car b) (car a) (cadr b))
      (<= (car b) (cadr a) (cadr b))))

(write (bool-map-sum data overlap))
(newline)
