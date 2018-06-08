#lang racket

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

(define (group-idx x y)
  (+ (* 3 (floor (/ y 3))) (floor (/ x 3))))

(define (lookup sudoku x y)
  (nth (row sudoku y) x))

(define (group sudoku idx)
  (let ((y-idxs (map (lambda (y) (+ y (* (floor (/ idx 3)) 3))) '(0 1 2)))
        (x-idxs (map (lambda (x) (+ x (* (modulo idx 3) 3))) '(0 1 2))))
    (flatten (map (lambda (x) (map (lambda (y) (lookup sudoku x y)) y-idxs)) x-idxs))))

;;

(define numbers '(1 2 3 4 5 6 7 8 9))

(define (unused-number list)
  (filter (lambda (n) (not (include? list n))) numbers))

(define (candidate sudoku x y)
  (if (not (equal? 0 (lookup sudoku x y)))
      '()
      (unused-number (flatten (list (row sudoku y) (col sudoku x) (group sudoku (group-idx x y)))))))