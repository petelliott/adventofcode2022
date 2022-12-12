(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme cxr)
        (util)
        (srfi 1))

(define (input)
  (define l (read-line))
  (if (eof-object? l)
      '()
      (cons (list->vector
             (map (lambda (ch)
                    (case ch
                      ((#\E) 'E)
                      ((#\S) 'S)
                      (else ( - (char->integer ch) (char->integer #\a)))))
                  (string->list l)))
            (input))))

(define data (list->vector (input)))

(define (ref x y)
  (if (and (<= 0 x (- (vector-length (vector-ref data 0)) 1))
           (<= 0 y (- (vector-length data) 1)))
      (vector-ref (vector-ref data y) x)
      #f))

(define (find-point sym)
  (call/cc
   (lambda (return)
     (do ((y 0 (+ y 1))) ((>= y (vector-length data)))
       (do ((x 0 (+ x 1))) ((>= x (vector-length (vector-ref data 0))))
         (if (eq? (ref x y) sym)
             (return (list x y))))))))

(define (height v)
  (case v
    ((S) 0)
    ((E) 25)
    (else v)))

;; part 1

(define (bfs queue seen)
  (define data (queue-peek queue))
  (define dist (car data))
  (define point (cdr data))
  (define x (car point))
  (define y (cadr point))
  (define h (ref x y))
  (cond
   ((eq? (ref x y) 'E) dist)
   ((hash-ref seen point)
    (bfs (queue-pop queue) seen))
   (else
    (hash-set! seen point #t)
    (bfs (apply queue-push (queue-pop queue)
                (delete #f
                        (map (lambda (coord)
                               (and (apply ref coord)
                                    (<= (height (apply ref coord)) (+ (height h) 1))
                                    (cons (+ dist 1) coord)))
                             (list (list (+ x 1) y)
                                   (list (- x 1) y)
                                   (list x (+ y 1))
                                   (list x (- y 1))))))
         seen))))

(write (bfs (queue-push (make-queue) (cons 0 (find-point 'S))) (make-hash-table)))
(newline)

;; part 2

(define (bfs2 queue seen)
  (define data (queue-peek queue))
  (when data
    (let* ((dist (car data))
           (point (cdr data))
           (x (car point))
           (y (cadr point))
           (h (ref x y)))
      (cond
       ((hash-ref seen point)
        (bfs2 (queue-pop queue) seen))
       (else
        (hash-set! seen point dist)
        (bfs2 (apply queue-push (queue-pop queue)
                     (delete #f
                             (map (lambda (coord)
                                    (and (apply ref coord)
                                         (>= (height (apply ref coord)) (- (height h) 1))
                                         (cons (+ dist 1) coord)))
                                  (list (list (+ x 1) y)
                                        (list (- x 1) y)
                                        (list x (+ y 1))
                                        (list x (- y 1))))))
              seen))))))

(define seen (make-hash-table))
(bfs2 (queue-push (make-queue) (cons 0 (find-point 'E))) seen)

(define mind 10000000000000000)

(do ((y 0 (+ y 1))) ((>= y (vector-length data)))
  (do ((x 0 (+ x 1))) ((>= x (vector-length (vector-ref data 0))))
    (when (eq? (height (ref x y)) 0)
        (set! mind (min mind (hash-ref seen (list x y) 1000000000000))))))

(write mind)
(newline)
