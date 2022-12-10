(import (scheme base)
    (scheme write)
    (scheme read)
    (scheme cxr)
    (util)
    (srfi 1))

(define (input)
  (define opcode (read))
  (cond
   ((eof-object? opcode) '())
   ((eq? opcode 'addx) (cons (list opcode (read)) (input)))
   (else (cons (list opcode) (input)))))

(define data (input))

;; part 1

(define part1 0)

(define (measure cycle X)
  (set! part1 (+ part1 (* cycle X))))

(define points '(20 60 100 140 180 220))

(define (checkpoint cycle X)
  (when (member cycle points)
    (measure cycle X)))

(define (run-vm insts cycle X)
  (checkpoint cycle X)
  (if (null? insts)
      X
      (case (caar insts)
        ((noop) (run-vm (cdr insts) (+ cycle 1) X))
        ((addx)
         (checkpoint (+ cycle 1) X)
         (run-vm (cdr insts) (+ cycle 2) (+ X (cadar insts)))))))

(run-vm data 1 1)
(write part1)
(newline)

;; part 2

;(define screen (list->vector (map (lambda (i) (make-vector 40 #\.)) (iota 6))))
(define screen (make-vector (* 6 40) #\.))

(define (checkpoint2 cycle X)
  (if (= (modulo cycle 40) 0) (newline))
  (if (>= (+ (modulo cycle 40) 1) X (- (modulo cycle 40) 1))
      (display #\#)
      (display #\.)))

(define (run-vm2 insts cycle X)
  (checkpoint2 cycle X)
  (if (null? insts)
      X
      (case (caar insts)
        ((noop) (run-vm2 (cdr insts) (+ cycle 1) X))
        ((addx)
         (checkpoint2 (+ cycle 1) X)
         (run-vm2 (cdr insts) (+ cycle 2) (+ X (cadar insts)))))))

(run-vm2 data 0 1)
