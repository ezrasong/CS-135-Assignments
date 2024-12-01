;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname assignment05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 07, Question 3
;; ***************************************************

;; A Node is a (make-node char term? next)
;; where char is a Char,
;; term? is a Bool,
;; next is a Next

(define-struct node (char term? next))

;; A Next is one of:
;; - empty
;; - (cons Node Next)
;; where each Node in Next is a Node,
;; and the Nodes are sorted in alphabetically ascending order of char.

;; node-template: Node -> ...
(define (node-template n)
  (... (node-char n)
       (node-term? n)
       (next-template (node-next n))))

;; next-template: Next -> ...
(define (next-template n)
  (cond
    [(empty? n) (...)]
    [else
     (... (node-template (first n))
          (next-template (rest n)))]))

;; (b) 
;; create-tree: 
;; Consumes a list of words and produces a Node that acts as the root-node for the tree.
;; The root node has char #\space and term? false.
;; All letters are stored as uppercase characters.

;; create-tree: (listof String) -> Node
(define (create-tree word-list)
  (make-node #\space false
             (create-tree-helper word-list empty)))
;; (create-tree word-list) produces a root Node for a word tree, where each word in word-list 
;; is stored as a sequence of nodes in uppercase characters. 
;; The root node has a char of #\space and term? set to false.

;; create-tree-helper: (listof String) Next -> Next
;; Processes each word in word-list and inserts it into the Next list.
(define (create-tree-helper word-list next)
  (cond
    [(empty? word-list) next]
    [else
     (create-tree-helper (rest word-list)
       (insert-word (string->list (string-upcase (first word-list))) next))]))

;; insert-word: (listof Char) Next -> Next
;; Inserts the word represented by chars into the Next list.
(define (insert-word chars next)
  (insert-chars chars next))
;; (insert-word chars next) inserts the word represented by the list 
;; of characters chars into the Next list next.

;; insert-chars: (listof Char) Next -> Next
;; Inserts the sequence of characters into the Next list.
(define (insert-chars chars next)
  (cond
    [(empty? chars) next]
    [else
     (insert-char (first chars) (rest chars) next)]))
;; (insert-chars chars next) recursively inserts each 
;; character in the list chars into the Next list next.

;; insert-char: Char (listof Char) Next -> Next
;; Inserts the character c and the rest of the chars into the Next list.
(define (insert-char c rest-chars next)
  (cond
    [(empty? next)
     (cons (create-node c rest-chars) empty)]
    [(char=? c (node-char (first next)))
     (cons (update-node (first next) rest-chars) (rest next))]
    [(char<? c (node-char (first next)))
     (cons (create-node c rest-chars) next)]
    [else
     (cons (first next) (insert-char c rest-chars (rest next)))]))
;; (insert-char c rest-chars next) inserts the character c and the 
;; remaining characters rest-chars into the Next list next, maintaining alphabetical order.

;; create-node: Char (listof Char) -> Node
;; Creates a new Node with char c, term? appropriately set,
;; and next constructed from rest-chars.
(define (create-node c rest-chars)
  (cond
    [(empty? rest-chars)
     (make-node c true empty)]
    [else
     (make-node c false (insert-chars rest-chars empty))]))
;; (create-node c rest-chars) creates a new Node with the character c, 
;; setting term? to true if rest-chars is empty, and constructs the Next field from rest-chars.

;; update-node: Node (listof Char) -> Node
;; Updates the node n by inserting rest-chars into its next field.
(define (update-node n rest-chars)
  (cond
    [(empty? rest-chars)
     (make-node (node-char n) true (node-next n))]
    [else
     (make-node (node-char n)
                (node-term? n)
                (insert-chars rest-chars (node-next n)))]))
;; (update-node n rest-chars) updates the node n by 
;; inserting the characters rest-chars into its Next field.

;; Examples:
(check-expect (create-tree (list "FEW" "FUN"))
  (make-node #\space false (list
    (make-node #\F false (list
      (make-node #\E false (list
        (make-node #\W true empty)))
      (make-node #\U false (list
        (make-node #\N true empty))))))))

(check-expect (create-tree (list "FUN" "FAR"))
  (make-node #\space false (list
    (make-node #\F false (list
      (make-node #\A false (list
        (make-node #\R true empty)))
      (make-node #\U false (list
        (make-node #\N true empty))))))))

(check-expect (create-tree (list "FUN" "fun" "fUn" "F"))
  (make-node #\space false (list
    (make-node #\F true (list
      (make-node #\U false (list
        (make-node #\N true empty))))))))

;; (c) 
;; check: 
;; Consumes a word and the root-node of a word-tree, and yields true if the word
;; is stored in the word-tree, and false otherwise.

;; Examples:
(define dict (create-tree (list "FEW" "FUN")))
(check-expect (check "FEW" dict) true)
(check-expect (check "few" dict) true)
(check-expect (check "potatoes" dict) false)

;; check: String Node -> Bool
(define (check word root-node)
  (check-chars (string->list (string-upcase word)) (node-next root-node)))
;; (check word root-node) checks if the word word exists in the word tree 
;; starting from root-node, returning true if found, and false otherwise.

;; check-chars: (listof Char) Next -> Bool
;; Checks if the sequence of characters chars exists in the Next list.
(define (check-chars chars next)
  (cond
    [(empty? chars) false]
    [else
     (check-char (first chars) (rest chars) next)]))
;; (check-chars chars next) checks if the sequence of characters chars exists in the Next list next.

;; check-char: Char (listof Char) Next -> Bool
;; Checks if the character c and the rest of chars exist in the Next list.
(define (check-char c rest-chars next)
  (cond
    [(empty? next) false]
    [(char=? c (node-char (first next)))
     (cond
       [(empty? rest-chars)
        (node-term? (first next))]
       [else
        (check-chars rest-chars (node-next (first next)))] )]
    [(char<? c (node-char (first next)))
     false]
    [else
     (check-char c rest-chars (rest next))]))
;; (check-char c rest-chars next) checks if the character
;; c and the remaining characters rest-chars exist in the Next list next, 
;; maintaining alphabetical order to ensure efficient search.


