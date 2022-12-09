(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme cxr)
        (util)
        (srfi 1))

(define (input)
  (define move (read))
  (if (eof-object? move)
      '()
      (cons (list move (read)) (input))))

(define data (input))

(define-record-type point
  (p x y)
  point?
  (x px)
  (y py))

(define (2v f . rest)
  (p (apply f (map px rest))
     (apply f (map py rest))))

(define (point->list p)
  (list (px p) (py p)))

;; part 1

(define (correct-tail head tail)
  ;;(write (point->list head)) (write (point->list tail))
  ;;(newline)
  (define delta (2v - head tail))
  (cond
   ((and (= (abs (px delta)) 2) (= (abs (py delta)) 2))
    (p (+ (px tail) (/ (px delta) 2)) (+ (py tail) (/ (py delta) 2))))
   ((= (abs (px delta)) 2)
    (p (+ (px tail) (/ (px delta) 2)) (py head)))
   ((= (abs (py delta)) 2)
    (p (px head) (+ (py tail) (/ (py delta) 2))))
   ((> (max (abs (px delta)) (abs (py delta))) 2)
    (error "too far in one move"))
   (else tail)))

(define (do1 pt dir)
  (case dir
    ((U) (p (px pt) (+ (py pt) 1)))
    ((D) (p (px pt) (- (py pt) 1)))
    ((R) (p (+ (px pt) 1) (py pt)))
    ((L) (p (- (px pt) 1) (py pt)))))

(define (simulate1 head tail commands)
  (if (null? commands)
      (list (point->list tail))
      (let loop ((h head)
                 (t tail)
                 (n (cadar commands)))
        (if (= n 0)
            (simulate1 h t (cdr commands))
            (cons (point->list t)
                  (let ((nh (do1 h (caar commands))))
                    (loop nh (correct-tail nh t) (- n 1))))))))

(write (length (delete-duplicates (simulate1 (p 0 0) (p 0 0) data))))
(newline)

;; part 2

;; state printer -- not part of problem
(define (print-state head tails)
  (do ((y 12 (- y 1))) ((< y -12))
    (do ((x -12 (+ x 1))) ((> x 12))
      (display (cond
                ((equal? (list x y) head) #\H)
                ((member (list x y) tails) => (lambda (l) (- 10 (length l))))
                ((equal? (list x y) (list 0 0)) #\s)
                (else #\.))))
    (newline)))

(define (correct-tails head tails)
  (if (null? tails)
      '()
      (let ((ctail (correct-tail head (car tails))))
        (cons ctail (correct-tails ctail (cdr tails))))))

(define (simulate2 head tails commands)
  (if (null? commands)
      (list (point->list (last tails)))
      (let loop ((h head)
                 (t tails)
                 (n (cadar commands)))
        (if (= n 0)
            (simulate2 h t (cdr commands))
            (cons (point->list (last t))
                  (let ((nh (do1 h (caar commands))))
                    (loop nh (correct-tails nh t) (- n 1))))))))

(write (length (delete-duplicates (simulate2 (p 0 0) (repeat (p 0 0) 9) data))))
(newline)
