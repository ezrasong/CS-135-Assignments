;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname s) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2023
;; Assignment 08, Question 4
;; ***************************************************

(define-struct node (key left right))
;; A Node is a (make-node Nat BT BT)
;; A Binary Tree (BT) is one of:
;; * empty
;; * Node

;; tree-pred:
;; Consumes a predicate pred and produces a function that consumes a binary tree bt.
;; The produced function returns true if pred returns true on every key in bt; false otherwise.
;; If bt is empty, it returns true.

;; Examples:
(define t (make-node 5
                     (make-node 10 empty empty)
                     (make-node 15
                                 (make-node 20 empty empty)
                                 (make-node 33 empty empty))))

(check-expect ((tree-pred even?) t) false)
(check-expect ((tree-pred positive?) t) true)

;; tree-pred: (Nat -> Bool) -> (BT -> Bool)
(define (tree-pred pred)
  (local
    [(define (check-tree tree)
       (cond
         [(empty? tree) true]
         [else
          (and (pred (node-key tree))
               (check-tree (node-left tree))
               (check-tree (node-right tree)))]))]
    check-tree))
;; (tree-pred pred) consumes a predicate function pred and produces a new function. This new function:
;; Consumes a binary tree (bt).
;; Returns true if pred returns true for every key in the tree (bt) or if the tree is empty.
;; Returns false if pred returns false for any key in the tree.