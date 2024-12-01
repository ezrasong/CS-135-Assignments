;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname assignment06-question1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 06, Question 1
;; ***************************************************

;; my-list-ref:
;; Consumes a list of numbers 'lon' and a natural number 'idx', and produces the element
;; at index 'idx'. If 'idx' exceeds the length of the list, produces false.

;; Examples:
(check-expect (my-list-ref (list 1 2 3 4) 0) 1)
(check-expect (my-list-ref (list 5 4 3) 2) 3)
(check-expect (my-list-ref (list 2) 20) false)

;; my-list-ref: (listof Num) NatNum -> (anyof Num False)
(define (my-list-ref lon idx)
  (cond
    [(empty? lon) false]
    [(= idx 0) (first lon)]
    [else (my-list-ref (rest lon) (- idx 1))]))
;; (my-list-ref lon idx) produces the element at the specified idx in the list lon. 
;; If idx is greater than or equal to the length of the list, it returns false.

;; zip:
;; Consumes two lists 'lst1' and 'lst2' of the same length, and produces an association list
;; pairing elements from the two lists.

;; Examples:
(check-expect (zip (list 1 2 3 4) (list "a" "b" "c" "d"))
              (list (list 1 "a") (list 2 "b") (list 3 "c") (list 4 "d")))
(check-expect (zip empty empty) empty)

;; zip: (listof Any) (listof Any) -> (listof (list Any Any))
(define (zip lst1 lst2)
  (cond
    [(and (empty? lst1) (empty? lst2)) empty]
    [else (cons (list (first lst1) (first lst2))
                (zip (rest lst1) (rest lst2)))]))
;; (zip lst1 lst2) produces an association list pairing elements from lst1 and lst2.

;; list-xor:
;; Consumes two sorted lists 'lon1' and 'lon2' and produces a sorted list of elements that
;; appear in either but not both of the lists (symmetric difference).

;; Examples:
(check-expect (list-xor (list 1 3 5) (list 2 3 4)) (list 1 2 4 5))
(check-expect (list-xor (list 1 2 3) (list 1 2 3)) empty)
(check-expect (list-xor (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))

;; list-xor: (listof Num) (listof Num) -> (listof Num)
(define (list-xor lon1 lon2)
  (cond
    [(empty? lon1) lon2]
    [(empty? lon2) lon1]
    [(= (first lon1) (first lon2))
     (list-xor (rest lon1) (rest lon2))]
    [(< (first lon1) (first lon2))
     (cons (first lon1) (list-xor (rest lon1) lon2))]
    [else
     (cons (first lon2) (list-xor lon1 (rest lon2)))]))
;; (list-xor lon1 lon2) produces a sorted list of elements that appear in either but not both of 
;; the lists lon1 and lon2 (symmetric difference).