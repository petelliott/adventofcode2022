(define-library (util)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (srfi 1)
          (srfi 28)
          (srfi 69))
  (export string-split
          list-set
          list-head
          string->object
          repeat
          maxn
          queue? make-queue queue-push queue-peek queue-pop
          memoize
          memo-stats
          bfs
          cross)
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

    (define (repeat d n)
    (if (= n 0)
        '()
        (cons d (repeat d (- n 1)))))

    (define (1maxn item maxes)
      (cond
       ((null? maxes) '())
       ((> item (car maxes))
        (cons item (reverse (cdr (reverse maxes)))))
       (else (cons (car maxes) (1maxn item (cdr maxes))))))

    (define (maxn lst n)
      (fold 1maxn (make-list n 0) lst))

    ;; immutable queues

    (define-record-type queue
      (construct-queue front back)
      queue?
      (front queue-front)
      (back queue-back))

    (define (make-queue)
      (construct-queue '() '()))

    (define (list-push list items)
      (if (null? items)
          list
          (list-push (cons (car items) list) (cdr items))))

    (define (queue-push queue . items)
      (construct-queue (queue-front queue)
                       (list-push (queue-back queue) items)))

    (define (queue-peek queue)
      (if (null? (queue-front queue))
          (if (null? (queue-back queue))
              #f
              (last (queue-back queue)))
          (car (queue-front queue))))

    (define (queue-pop queue)
      (if (null? (queue-front queue))
          (if (null? (queue-back queue))
              queue
              (queue-pop (construct-queue (reverse (queue-back queue)) '())))
          (construct-queue (cdr (queue-front queue)) (queue-back queue))))

    (define notfound '7048814c-c63b-4856-ab8e-e22c66391e1d)

    (define n 0)
    (define hits 0)
    (define (memoize proc)
      (define table (make-hash-table))
      (lambda args
        (define hash-r (hash-table-ref table args (lambda () notfound)))
        (if (eq? hash-r notfound)
            (let ((r (apply proc args)))
              (hash-table-set! table args r)
              (set! n (+ n 1))
              r)
            (begin
              (set! n (+ n 1))
              (set! hits (+ hits 1))
              hash-r))))

    (define (memo-stats)
      (display (format "~a calls, ~a hitrate\n"
                       n (if (= n 0) 0 (inexact (/ hits n))))))

    (define (bfs start end cproc)
      (define seen (make-hash-table))
      (let loop ((queue (queue-push (make-queue) (cons 0 start))))
        (let* ((qe (queue-peek queue))
               (dist (car qe))
               (node (cdr qe)))
          (cond
           ((equal? node end) dist)
           ((hash-table-exists? seen node)
            (loop (queue-pop queue)))
           (else
            (hash-table-set! seen node #t)
            (loop (apply queue-push (queue-pop queue)
                         (map (lambda (n) (cons (+ dist 1) n))
                              (cproc node)))))))))

    (define (cross l1 l2)
      (if (null? l1)
          '()
          (append (map (lambda (o) (list (car l1) o)) l2)
                  (cross (cdr l1) l2))))

    ))
