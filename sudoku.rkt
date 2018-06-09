#lang racket

(require racket/control)
(require srfi/1) ;iota

(define example
  '((5 3 0 0 7 0 0 0 0)
    (6 0 0 1 9 5 0 0 0)
    (0 9 8 0 0 0 0 6 0)
    (8 0 0 0 6 0 0 0 3)
    (4 0 0 8 0 3 0 0 1)
    (7 0 0 0 2 0 0 0 6)
    (0 6 0 0 0 0 2 8 0)
    (0 0 0 4 1 9 0 0 5)
    (0 0 0 0 8 0 0 7 9)
    ))

(define (include? list elem)
  (if (null? list) #f
      (if (equal? (car list) elem)
          #t
          (include? (cdr list) elem))))

(define (row sudoku y) ; y is zero-origin.
  (if (equal? y 0) (car sudoku)
      (row (cdr sudoku) (- y 1))))

(define (nth list n) ; n is zero-origin.
  (if (equal? n 0)
      (car list)
      (nth (cdr list) (- n 1))))

(define (col sudoku x)
  (map (lambda (row) (nth row x)) sudoku))

(define (lookup sudoku x y)
  (nth (row sudoku y) x))

(define (group sudoku x y)
  (let ((x-idxs (map (lambda (xi) (+ (* (floor (/ x 3)) 3) xi)) '(0 1 2)))
        (y-idxs (map (lambda (yi) (+ (* (floor (/ y 3)) 3) yi)) '(0 1 2))))
    (flatten (map (lambda (x) (map (lambda (y) (lookup sudoku x y)) y-idxs)) x-idxs))))

;;

(define numbers '(1 2 3 4 5 6 7 8 9))

(define (unused-number list)
  (filter (lambda (n) (not (include? list n))) numbers))

(define (candidates sudoku x y)
  (if (not (equal? 0 (lookup sudoku x y)))
      '()
      (unused-number (flatten (list (row sudoku y) (col sudoku x) (group sudoku x y))))))
;(candidates example 2 0)

(define (substitute list n elem)
  (if (equal? n 0)
      (cons elem (cdr list))
      (cons (car list) (substitute (cdr list) (- n 1) elem))))

(define (apply-candidate sudoku x y n)
  (if (not (equal? (lookup sudoku x y) 0))
      (error "is not emply") '())
  (if (not (include? (candidates sudoku x y) n))
      (error "is not candidate") '())
  (substitute sudoku y (substitute (row sudoku y) x n)))
;(apply-candidate example 8 0 4)

(define (flatten-one list)
  (foldr append '() list))

(define (search-space sudoku)
  (filter (lambda (pair) (equal? 0 (lookup sudoku (car pair) (cdr pair))))
          (flatten-one (map (lambda (x) (map (lambda (y) (cons x y)) (iota 9))) (iota 9)))))

(define (solutions sudoku)
  (let ((search-space (search-space sudoku)))
    (if (empty? search-space)
        (list sudoku) ;solved!
        (let* ((pair (car search-space))
               (x (car pair))
               (y (cdr pair))
               (candidates (candidates sudoku x y)))
          (if (empty? candidates)
              '()
              (flatten-one (map (lambda (cand) (solutions (apply-candidate sudoku x y cand))) candidates)))))))
;(solutions example)