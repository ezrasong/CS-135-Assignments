;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 03, Question 1
;; ***************************************************

(define-struct point (x y))

;; Make a function that returns a new point after doing multiplication with two inputted points

;; Point-mult Examples
(define p1 (make-point 1 2))
(define p2 (make-point 3 4))
(check-expect (point-mult p1 p2) (make-point -5 10))

;; point-mult: Point Point -> Point
(define (point-mult p1 p2)
  ;; (point-pult p1 p2) produces the point: (x1*x2)-(y1*y2), (x1*y2)+(x2*y1)
  (make-point (- (* (point-x p1) (point-x p2)) (* (point-y p1) (point-y p2)))
              (+ (* (point-x p1) (point-y p2)) (* (point-x p2) (point-y p1)))))

;; (point-mult p1 p2) returns a new point after doing multiplication with two inputted points

;; Tests
(define p3 (make-point 0 0))
(define p4 (make-point 5 7))
(check-expect (point-mult p3 p4) (make-point 0 0))

(define p5 (make-point 2 3))
(define p6 (make-point 5 7))
(check-expect (point-mult p5 p6) (make-point -11 29))

;; Make a function that returns a new point after dividing two inputted points

;; Examples
(define a1 (make-point 2 1))
(define a2 (make-point 1 2))
(check-expect (point-div a1 a2) (make-point 4/5 -3/5))

;; point-div: Point Point -> Point
;; x2 and y2 cannot be zero, as division by zero is undefined
(define (point-div p1 p2)
  ;; (point-div p1 p2) produces the point: (x1*x2 + y1*y2)/(x2^2 + y2^2),
  ;;                                       (y1*x2 - x1*y2)/(x2^2 + y2^2)
  (make-point (/ (+ (* (point-x p1) (point-x p2)) (* (point-y p1) (point-y p2)))
                 (+ (* (point-x p2) (point-x p2)) (* (point-y p2) (point-y p2))))
              (/ (- (* (point-y p1) (point-x p2)) (* (point-x p1) (point-y p2)))
                 (+ (* (point-x p2) (point-x p2)) (* (point-y p2) (point-y p2))))))

;; (point-div p1 p2) returns a new point after dividing two inputted points

;; Tests
(define a3 (make-point 4 2))
(define a4 (make-point 2 2))
(check-expect (point-div a3 a4) (make-point 3/2 -1/2))

(define a5 (make-point 0 0))
(define a6 (make-point 1 1))
(check-expect (point-div a5 a6) (make-point 0 0))
