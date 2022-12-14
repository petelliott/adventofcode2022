(import (scheme base)
    (scheme write)
    (scheme read)
    (scheme cxr)
    (util)
    (srfi 1))

(define-record-type monkey
  (make-monkey items op opand test ti fi inspections)
  monkey?
  (items mitems mset-items!)
  (op mop)
  (opand mopand)
  (test mtest)
  (ti mti)
  (fi mfi)
  (inspections minspections mset-inspections!))

(define (input)
  (if (eof-object? (read-line))
      '()
      (let ((itemline (read-line))
            (opline (read-line))
            (testline (read-line))
            (tline (read-line))
            (fline (read-line)))
        (read-line)
        (cons (make-monkey (map (lambda (s) (string->number (string-copy s 1)))
                                (string-split (string-copy itemline 17) #\,))
                           (string->symbol (string-copy opline 23 24))
                           (string->number (string-copy opline 25))
                           (string->number (string-copy testline 21))
                           (string->number (string-copy tline 29))
                           (string->number (string-copy fline 30))
                           0)
              (input)))))

(define monkeys (list->vector (input)))

(define (throw-to midx val)
  (define monk (vector-ref monkeys midx))
  (mset-items! monk (cons val (mitems monk))))

(define divisor (fold * 1 (map mtest (vector->list monkeys))))

(define (do-op monk val)
  (case (mop monk)
    ((*) (modulo (* val (or (mopand monk) val)) divisor))
    ((+) (modulo (+ val (or (mopand monk) val)) divisor))
    (else (error "unexpected op:" (mop monk)))))

(define (process-monkey monk)
  (mset-inspections! monk (+ (minspections monk) (length (mitems monk))))
  (for-each (lambda (val)
              (let ((worry (do-op monk val)))
                (if (= (modulo worry (mtest monk)) 0)
                    (throw-to (mti monk) worry)
                    (throw-to (mfi monk) worry))))
            (mitems monk))
  (mset-items! monk '()))

(define (do-round)
  (vector-for-each process-monkey monkeys))

(for-each (lambda (n) (do-round)) (iota 10000))

(write (fold * 1 (maxn (map minspections (vector->list monkeys)) 2)))
(newline)
