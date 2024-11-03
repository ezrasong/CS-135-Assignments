;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname symbol-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 05, Question 3
;; ***************************************************

;; make-symbol-lists: (listof Nat) Sym -> (listof (listof Sym))
;; Consumes a list of natural numbers and a symbol.
;; Produces a list where each inner list contains copies of the symbol, with length equal to the corresponding number.

;; Examples:
(check-expect (make-symbol-lists (list 2 1 3) 'X)
              (list (list 'X 'X) (list 'X) (list 'X 'X 'X)))
(check-expect (make-symbol-lists empty 'Y) empty)
(check-expect (make-symbol-lists (list 0 4) 'Z)
              (list empty (list 'Z 'Z 'Z 'Z)))

(define (make-symbol-lists nums sym)
  (cond
    [(empty? nums) empty]
    [else (cons (make-symbol-list (first nums) sym)
                (make-symbol-lists (rest nums) sym))]))

;; Helper Function:

;; make-symbol-list: Nat Sym -> (listof Sym)
;; Produces a list containing n copies of sym.
(define (make-symbol-list n sym)
  (cond
    [(zero? n) empty]
    [else (cons sym (make-symbol-list (- n 1) sym))]))
