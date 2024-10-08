;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 04, Question 2
;; ***************************************************

;; Function that returns true if the list has exactly 4 symbols (and possibly other values), 
;; false otherwise.

;; Examples
(check-expect
  (list-has-exactly-4-symbols (cons 'Prof (cons "Roh" (cons "is"
    (cons 'number (cons 1 empty)))))) false)

(check-expect
  (list-has-exactly-4-symbols (cons 'I (cons 'really (cons 'love
    (cons 'CS135 (cons "!" empty)))))) true)

;; list-has-exactly-4-symbols: (listof (anyof Num Str Bool Sym)) -> Bool
(define (list-has-exactly-4-symbols lst)
  ;; Checks if the number of symbols in the list is exactly 4
  (= (count-symbols lst) 4))

;; Helper function to count the number of symbols in the list
;; count-symbols: (listof (anyof Num Str Bool Sym)) -> Num
(define (count-symbols lst)
  (cond
    ;; If the list is empty, return 0
    [(empty? lst) 0]
    ;; If the first element is a symbol, add 1 and recurse on the rest of the list
    [(symbol? (first lst)) (+ 1 (count-symbols (rest lst)))]
    ;; Otherwise, recurse on the rest of the list without adding
    [else (count-symbols (rest lst))]))

;; (list-has-exactly-4-symbols lst) returns true if lst contains exactly 4 symbols, false otherwise.


;; Function that sums all the numbers in a list, ignoring non-numbers and inner lists.
;; If there are no numbers, it returns 0.

;; Examples
(check-expect
  (add-only-numbers (cons "CS" (cons 135 (cons 'is
    (cons "number" (cons 1 empty)))))) 136)

(check-expect
  (add-only-numbers (cons (cons 1 (cons 3 empty)) (cons "Hi"
    (cons 5 empty)))) 5)

;; add-only-numbers: (listof Any) -> Num
(define (add-only-numbers lst)
  (cond
    ;; If the list is empty, return 0
    [(empty? lst) 0]
    ;; If the first element is a number and not a list, add it to the sum
    [(and (number? (first lst)) (not (list? (first lst))))
     (+ (first lst) (add-only-numbers (rest lst)))]
    ;; Otherwise, ignore the first element and recurse on the rest
    [else (add-only-numbers (rest lst))]))

;; (add-only-numbers lst) returns the sum of all numbers in lst.
;; If lst contains no numbers, it returns 0.

;; Function that inserts a specified string before and after each occurrence of a target 
;; string in a list.

;; Examples
(check-expect
  (before-after "student" "hello" "welcome"
    (cons "smart" (cons "student" (cons "here" empty))))
  (cons "smart" (cons "hello" (cons "student"
    (cons "welcome" (cons "here" empty))))))

;; before-after: String String String (listof String) -> (listof String)
(define (before-after target before after lst)
  (cond
    ;; If the list is empty, return empty
    [(empty? lst) empty]
    ;; If the first element matches the target string
    [(string=? (first lst) target)
     ;; Insert 'before', then the target, then 'after', and recurse on the rest
     (cons before (cons (first lst) (cons after (before-after target before after (rest lst)))))]
    ;; Otherwise, keep the first element and recurse
    [else
     (cons (first lst) (before-after target before after (rest lst)))]))

;; (before-after target before after lst) returns a new list where whenever 'target' appears,
;; 'before' is inserted before it and 'after' is inserted after it.
;; If 'target' does not appear in lst, returns lst unchanged.

;; Function that checks if a given value exists in a list of numbers, strings, or symbols.

;; Examples
(check-expect (exists? 135 (cons 'CS (cons 135 (cons "rules" empty)))) true)
(check-expect (exists? 135 (cons 'CS (cons "135" (cons "then" (cons 136 empty))))) false)

;; exists?: (anyof Num Str Sym) (listof (anyof Num Str Sym)) -> Bool
(define (exists? val lst)
  (cond
    ;; If the list is empty, return false
    [(empty? lst) false]
    ;; If the value matches the first element
    [(value-equal? val (first lst)) true]
    ;; Otherwise, recurse on the rest of the list
    [else (exists? val (rest lst))]))

;; Helper function to compare two values of type Num, Str, or Sym
;; value-equal?: (anyof Num Str Sym) (anyof Num Str Sym) -> Bool
(define (value-equal? val1 val2)
  (cond
    ;; If both are numbers, compare numerically
    [(and (number? val1) (number? val2)) (= val1 val2)]
    ;; If both are strings, compare using string=?
    [(and (string? val1) (string? val2)) (string=? val1 val2)]
    ;; If both are symbols, compare using symbol=?
    [(and (symbol? val1) (symbol? val2)) (symbol=? val1 val2)]
    ;; If types do not match, return false
    [else false]))

;; (exists? val lst) returns true if val is in lst, false otherwise.

;; Function that removes all but the last occurrence of each element in a list,
;; maintaining the original order.

;; Example
(check-expect (remove-duplicates
  (cons 'happy (cons 3 (cons 'happy (cons 2
    (cons 'cs (cons 2 (cons "CS" (cons 2
      (cons 5 empty))))))))))
  (cons 3 (cons 'happy (cons 'cs (cons "CS" (cons 2 (cons 5 empty)))))))

;; remove-duplicates: (listof (anyof Num Str Sym)) -> (listof (anyof Num Str Sym))
(define (remove-duplicates lst)
  (cond
    ;; If the list is empty, return empty
    [(empty? lst) empty]
    ;; If the first element exists later in the list
    [(exists-in-list? (first lst) (rest lst))
     ;; Skip the first element and recurse on the rest
     (remove-duplicates (rest lst))]
    ;; Otherwise, keep the first element and recurse
    [else
     (cons (first lst) (remove-duplicates (rest lst)))]))

;; Helper function to check if a value exists in the list
;; exists-in-list?: (anyof Num Str Sym) (listof (anyof Num Str Sym)) -> Bool
(define (exists-in-list? val lst)
  (cond
    ;; If the list is empty, return false
    [(empty? lst) false]
    ;; If the value matches the first element
    [(value-equal? val (first lst)) true]
    ;; Otherwise, recurse on the rest of the list
    [else (exists-in-list? val (rest lst))]))

;; Reusing value-equal? from Problem 2(d)

;; (remove-duplicates lst) returns a new list with duplicates removed, 
;; keeping only the last occurrence of each element.
