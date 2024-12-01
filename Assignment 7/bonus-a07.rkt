;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname assignment06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 07, Question 4
;; ***************************************************

;; A Boolean is 0 or 1

;; dec->bin:
;; Converts a natural number n to its binary representation in reverse order.
;; Examples:
;; (dec->bin 4) => (list 0 0 1)
;; (dec->bin 0) => (list 0)

;; dec->bin: Nat -> (listof Boolean)
(define (dec->bin n)
  (cond
    [(= n 0) (list 0)]
    [else (dec->bin-helper n)]))

(define (dec->bin-helper n)
  (cond
    [(= n 0) empty]
    [else (cons (remainder n 2)
                (dec->bin-helper (quotient n 2)))]))

;; bin->dec: 
;; Converts a binary number (in reverse order) to its decimal representation.
;; Examples:
;; (bin->dec (list 0 0 1)) => 4
;; (bin->dec (list 1 0 1)) => 5

;; bin->dec: (listof Boolean) -> Nat
(define (bin->dec bits)
  (bin->dec-helper bits 1))

(define (bin->dec-helper bits factor)
  (cond
    [(empty? bits) 0]
    [else (+ (* (first bits) factor)
             (bin->dec-helper (rest bits) (* factor 2)))]))

;; Boolean functions:

;; my-not: Boolean -> Boolean
(define (my-not x)
  (cond
    [(= x 0) 1]
    [else 0]))

;; my-and: Boolean Boolean -> Boolean
(define (my-and x y)
  (cond
    [(= x 0) 0]
    [(= y 0) 0]
    [else 1]))

;; my-or: Boolean Boolean -> Boolean
(define (my-or x y)
  (cond
    [(= x 1) 1]
    [(= y 1) 1]
    [else 0]))

;; my-xor: Boolean Boolean -> Boolean
(define (my-xor x y)
  (cond
    [(= x y) 0]
    [else 1]))

;; half-adder: Boolean Boolean -> (list Sum Carry)
;; Takes two bits A and B and returns Sum and Carry
(define (half-adder A B)
  (list (my-xor A B) (my-and A B)))

;; full-adder: Boolean Boolean Boolean -> (list Sum Carry)
;; Takes bits A, B, Ci and returns Sum and Co
(define (full-adder A B Ci)
  (list (my-xor (my-xor A B) Ci)
        (my-or (my-or (my-and A B) (my-and B Ci)) (my-and A Ci))))

;; ripple-adder: (listof Boolean) (listof Boolean) Carry-in -> (listof Boolean)
;; Adds two binary numbers represented as lists of bits, with an initial carry-in.
(define (ripple-adder A B Ci)
  (cond
    [(and (empty? A) (empty? B) (= Ci 0))
     empty]
    [(and (empty? A) (empty? B))
     (list Ci)]
    [(empty? A)
     (cons (my-xor (my-xor 0 (first B)) Ci)
           (ripple-adder empty
                         (rest B)
                         (my-or (my-or (my-and 0 (first B)) (my-and (first B) Ci)) (my-and 0 Ci))))]
    [(empty? B)
     (cons (my-xor (my-xor (first A) 0) Ci)
           (ripple-adder (rest A)
                         empty
                         (my-or (my-or (my-and (first A) 0) (my-and 0 Ci)) (my-and (first A) Ci))))]
    [else
     (cons (my-xor (my-xor (first A) (first B)) Ci)
           (ripple-adder (rest A)
                         (rest B)
                         (my-or (my-or (my-and (first A) (first B)) (my-and (first B) Ci)) (my-and (first A) Ci))))]))

;; add: Nat Nat -> Nat
;; Adds two natural numbers a and b.
;; Examples:
;; (check-expect (add 13 5) 18)
;; (check-expect (add 135135135 424242) 135559377)
(define (add a b)
  (bin->dec (ripple-adder (dec->bin a) (dec->bin b) 0)))
