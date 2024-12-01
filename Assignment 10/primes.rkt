;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname primes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 10, Question 2
;; ***************************************************

;; primes: Nat -> (listof Nat)
;; (primes n) consumes a natural number n and returns a list of all prime numbers less than or
;; equal to n.
;; The primes are sorted from smallest to largest.

;; Examples:
(check-expect (primes 1) empty)
(check-expect (primes 2) '(2))
(check-expect (primes 10) '(2 3 5 7))
(check-expect (primes 20) '(2 3 5 7 11 13 17 19))

;; primes: Nat -> (listof Nat)
(define (primes n)
  ;; (primes n) produces a list of all prime numbers ≤ n using the Sieve of Eratosthenes.
  (local
    [
      ;; enumerate: Nat Nat -> (listof Nat)
      ;; (enumerate low high) produces a list of numbers from low to high inclusive.
      (define (enumerate low high)
        (cond
          [(> low high) empty]
          [else (cons low (enumerate (+ low 1) high))]))
      
      ;; sieve: (listof Nat) -> (listof Nat)
      ;; (sieve candidates) applies the Sieve of Eratosthenes algorithm to the list of candidates.
      (define (sieve candidates)
        (cond
          [(empty? candidates) empty]
          [else
           (cons (first candidates)
                 (sieve (filter (lambda (x)
                                  (not (= (remainder x (first candidates)) 0)))
                                (rest candidates))))]))
    ]
    (sieve (enumerate 2 n))))
