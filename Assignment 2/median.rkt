;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname median) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 02, Question 2
;; ***************************************************

;; Function that takes three numbers and returns the median
(define (median-of-3-simple a b c)
  (cond
    [(or (and (<= a b) (<= b c)) (and (<= c b) (<= b a))) b]
    [(or (and (<= b a) (<= a c)) (and (<= c a) (<= a b))) a]
    [else c]))

(check-expect (median-of-3-simple 1 2 3) 2)
(check-expect (median-of-3-simple 2 1 3) 2)
