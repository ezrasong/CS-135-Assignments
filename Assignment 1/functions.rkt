;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 01, Question 3
;; ***************************************************

;; Q3 a) Function Definition for manhattan-distance
(define (manhattan-distance x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

;; Q3 b) Function Definition for batter-slugging-average
(define (batter-slugging-average s d t hr ab)
  (/ (+ s (* 2 d) (* 3 t) (* 4 hr)) ab))

;; Q3 c) Function Definition for cone-area
(define (cone-area r h)
  (* (pi r) (+ r (sqrt (+ (sqr h) (sqr r))))))

;; Q3 d) Function Definition for escape speed
(define (escape M r)
  (sqrt (/ (* 2 6.674e-11 M) r)))

;; Q3 e) Function Definition for partition-size-approximation
(define (partition-size-approximation n)
  (* (/ 1 (* 4 n (sqrt 3))) (expt (exp (* pi (sqrt (/ (* 2 n) 3)))))))

;; Q3 f) Function Definition for (Black-Scholes formula
(define (d1 maturity rate volatility spot-price strike-price)
  (* (/ 1 (* volatility (sqrt maturity)))
     (+ (log (/ spot-price strike-price)) (* (+ rate (/ (sqr volatility) 2)) maturity))))
