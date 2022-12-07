(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme cxr)
        (util)
        (srfi 1))

(define-record-type file
  (make-file name size)
  file?
  (name file-name)
  (size file-size))

(define-record-type dir
  (make-dir name children)
  dir?
  (name dir-name)
  (children dir-children dir-set-children!))

(define (add-child! dir child)
  (dir-set-children! dir (cons child (dir-children dir))))

(define (nname dir-or-file)
  (if (file? dir-or-file)
      (file-name dir-or-file)
      (dir-name dir-or-file)))

(define (child dir name)
  (find (lambda (node) (equal? (nname node) name)) (dir-children dir)))

(define (read-ls curr)
  (read-line)
  (if (or (eof-object? (peek-char)) (equal? #\$ (peek-char)))
      '()
      (let ((a (read)) (b (read)))
        (add-child! curr (if (eq? a 'dir)
                             (make-dir b '())
                             (make-file b a)))
        (read-ls curr))))

(define (input dirstack)
  (when (not (eof-object? (read))) ;; $
    (case (read)
      ((cd)
       (let ((name (read)))
         (if (eq? name '..)
             (input (cdr dirstack))
             (input (cons (child (car dirstack) name) dirstack)))))
      ((ls)
       (read-ls (car dirstack))
       (input dirstack)))))

(define root (make-dir '/ '()))
(read) (read) (read)
(input (list root))

;; part 1

(define part1 0)
(define (count node)
  (if (file? node)
      (file-size node)
      (let ((size (fold + 0 (map count (dir-children node)))))
        (if (<= size 100000)
            (set! part1 (+ part1 size)))
        size)))

(define total (count root))
(write part1)
(newline)

;; part 2

(define needed (- 30000000 (- 70000000 total)))

(define part2 70000000)
(define (find-smallest node)
  (if (file? node)
      (file-size node)
      (let ((size (fold + 0 (map find-smallest (dir-children node)))))
        (when (<= needed size part2)
            (set! part2 size))
        size)))

(find-smallest root)
(write part2)
(newline)
