;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname function) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Here is a test case for the function f. It has the
;; wrong answer! Run the code and note what happens. Replace 100 with 
;; the correct answer.  Run the code again to confirm that the test 
;; case passes.  Save the file.
(check-expect (f 3) 13)

;; Here is the function definition. It computes f(x) = 3x + 4
(define (f x)
  (+ (* 3 x) 4))




;; Replace "*" with the name of the Racket function
;; that raises a number, x, to the power of the the second, y.
;; When correctly replaced, the test case will pass.
(check-expect (foo 2 3) 9)

(define (foo x y)
  (+ (expt x y) 1))

