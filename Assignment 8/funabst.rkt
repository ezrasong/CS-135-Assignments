;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname s) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2023
;; Assignment 08, Question 2
;; ***************************************************

;; or-pred:
;; Consumes a predicate function pred and a list lst, and produces true if pred returns 
;; true on any element of lst; false otherwise.

;; Examples:
(check-expect (or-pred even? empty) false)
(check-expect (or-pred odd? (list 6 10 4)) false)
(check-expect (or-pred string? (list 5 "wow")) true)

;; or-pred: (Any -> Bool) (listof Any) -> Bool
(define (or-pred pred lst)
  (cond
    [(empty? lst) false]
    [else (or (pred (first lst)) (or-pred pred (rest lst)))]))
;; (or-pred pred lst) consumes a predicate function pred and a list lst. It 
;; produces true if pred returns true for at least one element of lst; otherwise, it produces false.

;; map2argfn:
;; Consumes a list of functions fns (each taking two numbers) and a list nums containing two numbers.
;; Produces a list of results from applying each function in fns to the two numbers in nums.

;; Examples:
(check-expect (map2argfn (list + - * / list) (list 3 2))
              (list 5 1 6 1.5 (list 3 2)))

;; map2argfn: (listof (Num Num -> Any)) (list Num Num) -> (listof Any)
(define (map2argfn fns nums)
  (cond
    [(empty? fns) empty]
    [else (cons ((first fns) (first nums) (second nums))
                (map2argfn (rest fns) nums))]))
;; (map2argfn fns nums) consumes a list of functions fns (each taking two numbers) and a list 
;; nums containing exactly two numbers. It produces a list of results obtained by applying each 
;; function in fns to the two numbers in nums.

;; arranged?:
;; Consumes a list containing a predicate-function pred and a binary-relational-operator relop, 
;; and a list lst. Produces true if pred returns true on all elements in lst and relop returns 
;; true on all consecutive pairs; false otherwise.

;; Examples:
(check-expect (arranged? (list number? <) (list "wow")) false)
(check-expect (arranged? (list string? >) (list 'red "wow")) false)
(check-expect (arranged? (list string? >) (list "wow" 'red)) false)
(check-expect (arranged? (list string? string>?) (list "wow" "cs135" "amazing")) true)

;; arranged?: (list (Any -> Bool) (X X -> Bool)) (listof Any) -> Bool
(define (arranged? lst-pair lst)
  (cond
    [(empty? lst) true]
    [(not ((first lst-pair) (first lst))) false]
    [(empty? (rest lst)) true]
    [(not ((first lst-pair) (second lst))) false]
    [( (second lst-pair) (first lst) (second lst))
     (arranged? lst-pair (rest lst))]
    [else false]))
;; (arranged? lst-pair lst) consumes a list lst-pair containing a predicate function (pred) and a 
;; binary relational operator (relop), along with a list lst. It produces true if:
;; pred returns true for all elements in lst, and
;; relop returns true for all consecutive pairs in lst.
;; Otherwise, it produces false.

