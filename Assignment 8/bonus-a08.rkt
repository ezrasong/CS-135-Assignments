;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname s) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2023
;; Assignment 08, Bonus
;; ***************************************************

;; my-compose:
;; Consumes two functions f and g, and produces a function that applies g to x, then f to the result.

;; Example:
;; ((my-compose sqrt abs) -9) => 3

;; my-compose: (Y -> Z) (X -> Y) -> (X -> Z)
(define (my-compose f g)
  (lambda (x) (f (g x))))
;; (my-compose f g) consumes two functions, f and g, and produces a new function that applies g to 
;; an input x, then applies f to the result of g.

;; curry:
;; Consumes a two-argument function f, and produces a one-argument function that returns 
;; another function.

;; Example:
;; (((curry +) 2) 3) => 5

;; curry: ((X Y) -> Z) -> (X -> (Y -> Z))
(define (curry f)
  (lambda (x)
    (lambda (y)
      (f x y))))
;; (curry f) consumes a two-argument function f and produces a curried version of f. 
;; The curried function:
;; Takes one argument, x, and returns another function.
;; The returned function takes a second argument, y, and produces the result of applying f to x and y.

;; uncurry:
;; Opposite of curry; converts a curried function back to a two-argument function.

;; Example:
;; ((uncurry (curry +)) 2 3) => 5

;; uncurry: (X -> (Y -> Z)) -> ((X Y) -> Z)
(define (uncurry f)
  (lambda (x y)
    ((f x) y)))
;; (uncurry f) consumes a curried function f and produces a two-argument function. 
;; The uncurried function:
;; Takes two arguments, x and y, and applies the original curried function f as ((f x) y).

;; eat-apples:
;; Consumes a list of strings and removes all occurrences of "apple".

;; eat-apples: (listof String) -> (listof String)
(define eat-apples
  ((curry filter)
   (my-compose not
               ((curry string=?) "apple"))))
;; eat-apples is a function that consumes a list of strings and removes all occurrences
;; of the string "apple".