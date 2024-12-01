;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname s) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2023
;; Assignment 08, Question 3
;; ***************************************************

;; partition:
;; Consumes a predicate `pred` and a list `lst`, and produces a list of two lists:
;; The first list contains items in `lst` that satisfy `pred`;
;; The second list contains items that do not satisfy `pred`.
;; The order of items in each list is the same as in `lst`.

;; Examples:
(check-expect (partition even? (list 1 2 3 4 5))
               (list (list 2 4) (list 1 3 5)))

;; partition: (Any -> Bool) (listof Any) -> (list (listof Any) (listof Any))
(define (partition pred lst)
  (cond
    [(empty? lst) (list empty empty)]
    [(pred (first lst))
     (list (cons (first lst) (first (partition pred (rest lst))))
           (second (partition pred (rest lst))))]
    [else
     (list (first (partition pred (rest lst)))
           (cons (first lst) (second (partition pred (rest lst)))))]))
;; (partition pred lst) consumes a predicate function pred and a list lst. 
;; It produces a list containing two sublists:
;; The first sublist contains all elements from lst that satisfy pred.
;; The second sublist contains all elements from lst that do not satisfy pred.