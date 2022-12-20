(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme cxr)
        (srfi 1)
        (util))

(define-record-type valve
  (make-valve name rate tunnels)
  valve?
  (name valve-name)
  (rate valve-rate)
  (tunnels valve-tunnels))

(define valves (make-hash-table))

(define (input)
  (define l (read-line))
  (unless (eof-object? l)
    (let* ((s1 (string-split l #\;))
           (name (string->symbol (cadr (string-split (car s1) #\space))))
           (rate (string->number (cadr (string-split (car s1) #\=))))
           (tunnels (map (lambda (t)
                           (string->symbol (string-copy t 0 2)))
                         (list-tail (string-split (cadr s1) #\space) 5))))
      (hash-set! valves name (make-valve name rate tunnels))
      (input))))

(input)

(define (order a b)
  (< (hash a 4294967296) (hash b 4294967296)))

(define useful-valves
  (filter (lambda (valve)
            (or (> (valve-rate valve) 0)
                (eq? (valve-name valve) 'AA)))
          (hash-map->list (lambda (name valve) valve) valves)))

(define reduced-valves (make-hash-table))
(for-each (lambda (valve)
            (define new-tunnels
              (map (lambda (end)
                     (cons (valve-name end)
                           (bfs (valve-name valve) (valve-name end)
                                (lambda (name)
                                  (valve-tunnels (hash-ref valves name))))))
                   (filter (lambda (v) (not (eq? (valve-name valve)
                                                 (valve-name v))))
                           useful-valves)))
            (hash-set! reduced-valves
                       (valve-name valve)
                       (make-valve (valve-name valve)
                                   (valve-rate valve)
                                   new-tunnels)))
          useful-valves)

;; part 1

(define (part1 valve time open)
  (define rv (hash-ref reduced-valves valve))
  (define dt (if (= (valve-rate rv) 0) 0 1))
  (if (<= time 0)
      0
      (+ (* (valve-rate rv) (- time dt))
         (fold max 0 (map (lambda (v)
                            (part1 (car v) (- time (cdr v) dt)
                                   (sort (cons valve open) order)))
                          (filter (lambda (t)
                                    (not (member (car t) open)))
                                  (valve-tunnels rv)))))))

(write (part1 'AA 30 '()))
(newline)

;; part 2

(define (opts valve open)
  (filter (lambda (t)
            (not (member (car t) open)))))

(define (do1 valve open cont)
  (define rv (hash-ref reduced-valves valve))
  (if (or (= (valve-rate rv) 0) (member valve open))
      (fold max 0 (map (lambda (v) (cont (car v) (- time


(define (part2 queue time open)
  (define yv (hash-ref reduced-valves yvalve))
  (define ev (hash-ref reduced-valves yvalve))
  (define ydt (if (= (valve-rate yv) 0) 0 1))
  (define edt (if (= (valve-rate ev) 0) 0 1))
  (if (<= time 0)
      0
      (+ (* (valve-rate yv) (- time 1))
         (* (valve-rate ev) (- time 1))
         (fold max 0 (map (lambda (pair)
                            (part1 (car v) (- time (cdr v) dt)
                                   (sort (cons valve open) order)))
                          (cross
                           (filter (lambda (t)
                                     (not (member (car t) open)))
                                   (valve-tunnels rv)))))))))
