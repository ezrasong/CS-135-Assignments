;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 07, Question 1
;; ***************************************************

(define-struct component (name num subcomponents))
;; A Component is a (make-component Name Nat (listof Component))
;; Requires: num must be larger than 0

;; A Name is a Str

;; contains-component?: 
;; Consumes a Component c and a Name n, and produces true if c or any of its subcomponents
;; (recursively) has the name n, and false otherwise.

;; Examples:
(check-expect (contains-component? bike "hub") true)
(check-expect (contains-component? bike "brake") false)

;; contains-component?: Component Name -> Bool
(define (contains-component? c n)
  (cond
    [(string=? (component-name c) n) true]
    [else (contains-component?-list (component-subcomponents c) n)]))
;; (contains-component? c n) produces true if the Component c or any of its subcomponents 
;; (recursively) has the name n; otherwise, it produces false.

;; contains-component?-list:
;; Consumes a list of Components lst and a Name n, and produces true if any Component in lst
;; (or its subcomponents recursively) has the name n, and false otherwise.

;; contains-component?-list: (listof Component) Name -> Bool
(define (contains-component?-list lst n)
  (cond
    [(empty? lst) false]
    [else
     (or (contains-component? (first lst) n)
         (contains-component?-list (rest lst) n))]))
;; (contains-component?-list lst n) produces true if any Component in the list lst 
;; (or its subcomponents recursively) has the name n; otherwise, it produces false.