;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bin-sqrt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 10, Question 1
;; ***************************************************

;; bin-sqrt: Num Num -> Num
;; (bin-sqrt n tol) consumes a number n and a tolerance tol, and produces an approximation of √n
;; within tolerance tol, using the binary search method.

;; Examples:
(check-within (bin-sqrt 2 0.001) (sqrt 2) 0.001)
(check-within (bin-sqrt 9 0.001) 3 0.001)
(check-within (bin-sqrt 100 0.0001) 10 0.0001)
(check-within (bin-sqrt 0 0.001) 0 0.001)
(check-within (bin-sqrt 1 0.001) 1 0.001)

;; bin-sqrt: Num Num -> Num
(define (bin-sqrt n tol)
  ;; (bin-sqrt n tol) produces an approximation of √n within tolerance tol.
  (local
    [
      ;; helper: Num Num -> Num
      ;; (helper low high) recursively searches for √n between low and high.
      ;; Returns mid when |n - mid^2| ≤ tol.
      (define (helper low high)
        (cond
          [(<= (abs (- n (* (/ (+ low high) 2) (/ (+ low high) 2)))) tol)
           (/ (+ low high) 2)]
          [(> (* (/ (+ low high) 2) (/ (+ low high) 2)) n)
           (helper low (/ (+ low high) 2))]
          [else
           (helper (/ (+ low high) 2) high)]))
    ]
    (helper 0 n)))
