;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 02, Question 3
;; ***************************************************

;; Simplified version of Q3a
(define (q3a-simplified n a?)
  (cond
    [a? (if (>= n 0) (+ n 1) (- n 1))]
    [else 0]))

;; Simplified version of Q3b
(define (q3b-simplified a? b? c?)
  (cond
    [b? (if a? 'elm 'pine)]
    [(not c?) 'birch]
    [a? 'cedar]
    [else 'cherry]))

;; Simplified version of Q3c
(define (q3c-simplified a? b? c?)
  (cond
    [(and c? b?) 'spruce]
    [(and (not c?) (not a?)) 'fir]
    [(not b?) 'larch]
    [a? 'hazel]
    [else 'hickory]))
