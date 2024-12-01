;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hof-hills) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2023
;; Assignment 09, Question 3
;; ***************************************************

;; Question 3(a): pocket-change

(define (pocket-change lst)
  (foldr +
         0
         (map (lambda (coin)
                (cond
                  [(symbol=? coin 'nickel) 0.05]
                  [(symbol=? coin 'dime) 0.10]
                  [(symbol=? coin 'quarter) 0.25]
                  [(symbol=? coin 'loonie) 1.00]
                  [(symbol=? coin 'toonie) 2.00]
                  [else 0]))
              lst)))

;; Question 3(b): make-validator

(define (make-validator lst)
  (lambda (sym)
    (foldr (lambda (x acc) (or (symbol=? x sym) acc)) false lst)))

;; Question 3(c): remove-outliers

(define (remove-outliers lst)
  (local
    [
     (define n (length lst))
     (define mean (/ (foldr + 0 lst) n))
     (define sd
       (sqrt (/ (foldr (lambda (x acc) (+ acc (sqr (- x mean)))) 0 lst) n)))
     (define lower-bound (- mean (* 2 sd)))
     (define upper-bound (+ mean (* 2 sd)))
     ]
    (filter (lambda (x) (and (> x lower-bound) (< x upper-bound))) lst)))

;; Question 3(d): primes

(define (primes n)
  (filter
    (lambda (x)
      (not
        (foldr (lambda (d acc)
                 (or (= (remainder x d) 0) acc))
               false
               (build-list (- x 2) (lambda (i) (+ i 2))))))
    (build-list (- n 1) (lambda (i) (+ i 2)))))
