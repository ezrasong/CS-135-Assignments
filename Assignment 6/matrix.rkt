;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname assignment06-question2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 06, Question 2
;; ***************************************************


;; A Matrix is a (listof Row)
;; A Row is a (listof Number)
;; All Rows in a Matrix have the same length.

;; Example Matrix M:
(define M (list (list 1 2 3 4)
                (list 5 6 7 8)))

;; matrix-item: 
;; Consumes a Matrix 'M', a row number 'row', and a column number 'col',
;; and produces the item at that row and column position.
;; Indices start at 0.

;; Examples:
(check-expect (matrix-item M 1 2) 7)
(check-expect (matrix-item M 0 3) 4)
(check-expect (matrix-item M 0 0) 1)

;; Helper function my-list-ref: (listof Any) NatNum -> Any
;; Returns the element at index 'idx' in the list 'lst'.
(define (my-list-ref lst idx)
  (cond
    [(empty? lst) (error "Index out of bounds")]
    [(= idx 0) (first lst)]
    [else (my-list-ref (rest lst) (- idx 1))]))

;; matrix-item: Matrix NatNum NatNum -> Number
(define (matrix-item M row col)
  (my-list-ref (my-list-ref M row) col))
;;(matrix-item M row col) produces the item located at the specified row and col in matrix M.

;; matrix-col: 
;; Consumes a Matrix 'M' and a column number 'col', and produces that column as a list.

;; Examples:
(check-expect (matrix-col M 2) (list 3 7))
(check-expect (matrix-col M 0) (list 1 5))
(check-expect (matrix-col M 3) (list 4 8))

;; matrix-col: Matrix NatNum -> (listof Number)
(define (matrix-col M col)
  (cond
    [(empty? M) empty]
    [else (cons (my-list-ref (first M) col)
                (matrix-col (rest M) col))]))
;; (matrix-col M col) produces the specified column col from matrix M as a list of numbers.

;; matrix-transpose: 
;; Consumes a Matrix 'M' and produces its transpose.

;; Examples:
(check-expect (matrix-transpose M)
              (list (list 1 5)
                    (list 2 6)
                    (list 3 7)
                    (list 4 8)))

;; matrix-transpose: Matrix -> Matrix
(define (matrix-transpose M)
  (cond
    [(empty? (first M)) empty]
    [else
     (cons (matrix-col M 0)
           (matrix-transpose (matrix-remove-first-column M)))]))
;; (matrix-transpose M) produces the transpose of the matrix M, flipping rows and columns.

;; Helper function matrix-remove-first-column: Matrix -> Matrix
;; Removes the first column from each row in the Matrix.
(define (matrix-remove-first-column M)
  (cond
    [(empty? M) empty]
    [else (cons (rest (first M))
                (matrix-remove-first-column (rest M)))]))

;; matrix-multiply:
;; Consumes two matrices 'A' and 'B', and produces the result of multiplying them.

;; Example:
(define A (list (list 1 2 3)
                (list 4 5 6))) 

(define B (list (list 7 8)
                (list 9 10)
                (list 11 12))) 


(check-expect (matrix-multiply A B)
              (list (list 58 64)
                    (list 139 154)))

;; matrix-multiply: Matrix Matrix -> Matrix
(define (matrix-multiply A B)
  (cond
    [(empty? A) empty]
    [else
     (cons (multiply-row-by-columns (first A) (matrix-transpose B))
           (matrix-multiply (rest A) B))]))
;; (matrix-multiply A B) produces the result of multiplying matrix A with matrix B.

;; Helper function multiply-row-by-columns: Row (listof Row) -> Row
;; Multiplies a row by each column (as rows) in 'columns', producing a new row.
(define (multiply-row-by-columns row columns)
  (cond
    [(empty? columns) empty]
    [else
     (cons (dot-product row (first columns))
           (multiply-row-by-columns row (rest columns)))]))

;; Helper function dot-product: Row Row -> Number
;; Computes the dot product of two rows (vectors) of numbers.
(define (dot-product v1 v2)
  (cond
    [(or (empty? v1) (empty? v2)) 0]
    [else
     (+ (* (first v1) (first v2))
        (dot-product (rest v1) (rest v2)))]))
