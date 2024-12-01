;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hof-mountains) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2023
;; Assignment 09, Question 4
;; ***************************************************

;; Question 4(a): min-max

(define (min-max comp lst)
  (foldr (lambda (x acc)
           (list
            (cond
              [(comp x (first acc)) x]
              [else (first acc)])
            (cond
              [(comp (second acc) x) x]
              [else (second acc)])))
         (list (first lst) (first lst))
         (rest lst)))

;; Question 4(b): in-order?

(define (in-order? comp lst)
  (cond
    [(empty? lst) true]
    [(empty? (rest lst)) true]
    [else
     (foldr (lambda (a b acc)
              (and (comp a b) acc))
            true
            lst
            (rest lst))]))

;; Question 4(c): slice

(define (slice a b lst)
  (map second
       (filter (lambda (pair)
                 (and (>= (first pair) a) (<= (first pair) b)))
               (map list
                    (build-list (length lst) (lambda (i) i))
                    lst))))

;; Question 4(d): split-n

(define (split-n n lst)
  (map
   (lambda (k)
     (map second
          (filter (lambda (pair)
                    (= (modulo (first pair) n) k))
                  (map list
                       (build-list (length lst) (lambda (i) i))
                       lst))))
   (build-list n (lambda (k) k))))
