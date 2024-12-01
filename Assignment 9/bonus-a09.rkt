;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus-a09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2023
;; Assignment 09, Bonus 
;; ***************************************************

;; 5(a): subsets

(define (subsets lst)
  (cond
    [(empty? lst) (list empty)]
    [else
     (append (subsets (rest lst))
             (map (lambda (s) (cons (first lst) s))
                  (subsets (rest lst))))]))

;; 5(b): subsets-hof

(define (subsets-hof lst)
  (foldr (lambda (x acc)
           (foldr cons acc (map (lambda (s) (cons x s)) acc)))
         (list empty)
         lst))

