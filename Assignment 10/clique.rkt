;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname clique) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 10, Question 3
;; ***************************************************

;; Definitions from Module 18:

;; A Node is a Sym
;; A Graph is one of:
;; * empty
;; * (cons (list v (list w_1 ... w_n)) g)
;;   where g is a Graph
;;   v, w_1, ..., w_n are Nodes
;;   v is the in-neighbour to w_1 ... w_n in the Graph
;;   v does not appear as an in-neighbour in g

;; neighbours : Node Graph -> (listof Node)
;; Returns the list of nodes that are in-neighbours of the given node.
(define (neighbours v g)
  (cond
    [(empty? g) empty]
    [(symbol=? (first (first g)) v) (second (first g))]
    [else (neighbours v (rest g))]))

;; is-clique? : (listof Node) Graph -> Bool
;; (is-clique? lon g) returns true if the nodes in lon form a clique in graph g, false otherwise.
;; A clique is a set of nodes where every node is connected to every other node in both directions.

;; Examples:

;; Sample Graph:
(define g1 '((A (B C D)) (B (A C)) (C (A B)) (D ())))

;; Tests:
(check-expect (is-clique? empty g1) true)              ;; Empty set is a clique
(check-expect (is-clique? '(A) g1) true)               ;; Singleton set is a clique
(check-expect (is-clique? '(A B) g1) true)
(check-expect (is-clique? '(A B C) g1) true)
(check-expect (is-clique? '(A B D) g1) false)          ;; D is not connected to B
(check-expect (is-clique? '(B C) g1) true)
(check-expect (is-clique? '(A D) g1) false)            ;; D is not connected to A
(check-expect (is-clique? '(D) g1) true)               ;; D alone is a clique
(check-expect (is-clique? '(A B C D) g1) false)        ;; D is not connected to others

;; is-clique? : (listof Node) Graph -> Bool
(define (is-clique? lon g)
  ;; Determines if lon forms a clique in graph g.
  (cond
    [(empty? lon) true]
    [else
     (local
       [
         ;; member? : Node (listof Node) -> Bool
         ;; Checks if v is in lon.
         (define (member? v lon)
           (cond
             [(empty? lon) false]
             [(symbol=? v (first lon)) true]
             [else (member? v (rest lon))]))
         
         ;; all-in? : (listof Node) (listof Node) -> Bool
         ;; Checks if all nodes in sublist are in lst.
         (define (all-in? sublist lst)
           (cond
             [(empty? sublist) true]
             [else
              (and (member? (first sublist) lst)
                   (all-in? (rest sublist) lst))]))
         
         ;; remove-node : Node (listof Node) -> (listof Node)
         ;; Removes node v from lon.
         (define (remove-node v lon)
           (cond
             [(empty? lon) empty]
             [(symbol=? v (first lon)) (rest lon)]
             [else (cons (first lon) (remove-node v (rest lon)))]))
         
         ;; check-node : Node (listof Node) -> Bool
         ;; Checks if node u is connected to all nodes in rest-lon.
         (define (check-node u rest-lon)
           (all-in? rest-lon (neighbours u g)))
         
         ;; check-all : (listof Node) -> Bool
         ;; Checks if every node in lon is connected to all other nodes in lon.
         (define (check-all lon)
           (cond
             [(empty? lon) true]
             [else
              (and (check-node (first lon) (remove-node (first lon) lon))
                   (check-all (rest lon)))]))
       ]
       (check-all lon))]))
