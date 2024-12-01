;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname s) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2023
;; Assignment 08, Question 5
;; ***************************************************

;; nested-listof-X-template: (nested-listof X) -> Any
; (define (nested-listof-X-template nlst)
;   (cond
;     [(empty? nlst) ...]
;     [(list? (first nlst))
;      ... (nested-listof-X-template (first nlst)) ...
;      ... (nested-listof-X-template (rest nlst)) ...]
;     [else
;      ... (first nlst) ...
;      ... (nested-listof-X-template (rest nlst)) ...]))

;; nested-filter:
;; Consumes a predicate function pred and a nested list nlst, and removes every element
;; where pred returns false, including inside any nested lists.

;; Examples:
(check-expect (nested-filter number? '(1 "a" (2 "b") 3)) '(1 (2) 3))
(check-expect (nested-filter even? '(1 2 (3 4 (5 6)) 7)) '(2 (4 (6))))
(check-expect (nested-filter symbol? '(apple (banana "cherry") 42)) '(apple (banana)))

;; nested-filter: (X -> Bool) (nested-listof Any) -> (nested-listof Any)
(define (nested-filter pred nlst)
  (cond
    [(empty? nlst) empty]
    [(list? (first nlst))
     (cons (nested-filter pred (first nlst))
           (nested-filter pred (rest nlst)))]
    [(pred (first nlst))
     (cons (first nlst) (nested-filter pred (rest nlst)))]
    [else
     (nested-filter pred (rest nlst))]))
;; (nested-filter pred nlst) consumes a predicate function pred and a nested list nlst. 
;; It removes elements from nlst for which pred returns false, including inside any 
;; nested sublists, preserving the structure of the nested list.

;; not-ruth?: Symbol -> Bool
(define (not-ruth? x)
  (not (symbol=? x 'ruth)))
;; (not-ruth? x) returns true if x is not the symbol 'ruth, and false otherwise.

;; ruthless:
;; Consumes a nested list of symbols and removes all instances of 'ruth.

;; Example:
(check-expect
 (ruthless '(rabbit (apple pluto (ruth blue) ruth) hello))
 '(rabbit (apple pluto (blue)) hello))

;; ruthless: (nested-listof Symbol) -> (nested-listof Symbol)
(define (ruthless nlst)
  (nested-filter not-ruth? nlst))
;; (ruthless nlst) consumes a nested list of symbols nlst and removes all instances of 
;; the symbol 'ruth from it, using the nested-filter function.

;; keep-between:
;; Consumes two numbers a and b, and a nested list of numbers nlst.
;; Produces a nested list, keeping only the values between a and b inclusive.

;; Example:
(check-expect (keep-between 5 10 '(1 3 5 (7 9 10) (8 (3 4)) 8 15))
              '(5 (7 9 10) (8 ()) 8))

;; keep-between: Num Num (nested-listof Num) -> (nested-listof Num)
(define (keep-between a b nlst)
  (local
    [(define (between? x)
       (and (>= x a) (<= x b)))]
    (nested-filter between? nlst)))
;; (keep-between a b nlst) consumes two numbers a and b, and a nested list of 
;; numbers nlst. It keeps only the elements of nlst that are between a and b (inclusive), 
;; preserving the structure of the nested list.

;; nested-cleanup:
;; Consumes a (nested-listof Any) and removes all empty lists anywhere in it.
;; If there are no non-list elements anywhere in the list, it produces false.

;; Examples:
(check-expect (nested-cleanup '(1 () 2 () () 3)) '(1 2 3))
(check-expect (nested-cleanup '(1 (()()) 2 ((3 () (()))) )) '(1 2 ((3))))
(check-expect (nested-cleanup '(()(()())(())())) false)

;; nested-cleanup: (nested-listof Any) -> (nested-listof Any) or false
(define (nested-cleanup nlst)
  (local
    [(define result
       (cond
         [(empty? nlst) #f]
         [(list? (first nlst))
          (local
            [(define cleaned-first (nested-cleanup (first nlst)))
             (define cleaned-rest (nested-cleanup (rest nlst)))]
            (cond
              [(and (list? cleaned-first) (list? cleaned-rest)) (cons cleaned-first cleaned-rest)]
              [(list? cleaned-first) (list cleaned-first)]
              [(list? cleaned-rest) cleaned-rest]
              [else #f]))]
         [else
          (local
            [(define cleaned-rest (nested-cleanup (rest nlst)))]
            (cond
              [(list? cleaned-rest) (cons (first nlst) cleaned-rest)]
              [else (list (first nlst))]))]))]
    (cond
      [(list? result) result]
      [else false])))
;; (nested-cleanup nlst) consumes a nested list nlst and removes all empty lists (()) from it, 
;; preserving the structure. If the result contains no non-list elements, it returns false.

;; nested-map: (Num -> Num) (nested-listof Num) -> (nested-listof Num)
(define (nested-map f nlst)
  (cond
    [(empty? nlst) empty]
    [(list? (first nlst))
     (cons (nested-map f (first nlst))
           (nested-map f (rest nlst)))]
    [else
     (cons (f (first nlst))
           (nested-map f (rest nlst)))]))
;; (nested-map f nlst) consumes a function f and a nested list nlst. It applies f to every 
;; non-list element in nlst, preserving the structure of the nested list.

;; nested-apply:
;; Consumes a list of functions fns and a nested list nlst.
;; Produces a list where each element is the result of applying each function to nlst.

;; Example:
(check-expect (nested-apply (list abs floor) '(1.2 (-2 (3.5)) ()))
              (list '(1.2 (2 (3.5)) ()) '(1 (-2 (3)) ())))

;; nested-apply: (listof (Num -> Num)) (nested-listof Num) -> (listof (nested-listof Num))
(define (nested-apply fns nlst)
  (cond
    [(empty? fns) empty]
    [else
     (cons (nested-map (first fns) nlst)
           (nested-apply (rest fns) nlst))]))
;; (nested-apply fns nlst) consumes a list of functions fns and a nested list nlst. 
;; It produces a list where each element is the result of applying a function in fns 
;; to every non-list element of nlst, preserving the structure.