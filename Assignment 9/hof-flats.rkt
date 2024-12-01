;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hof-flats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2023
;; Assignment 09, Question 2
;; ***************************************************

;; Question 2(a): absolutely-odd

(define (absolutely-odd lst)
  (foldr + 0 (map abs (filter odd? lst))))

;; Question 2(b): count-n

(define (count-n n lst)
  (length (filter (lambda (x) (= x n)) lst)))

;; Question 2(c): unzip

(define (unzip lst)
  (list (map first lst) (map second lst)))

;; Question 2(d): dedup

(define (dedup lst)
  (foldr
    (lambda (x acc)
      (cond
        [(not (empty? (filter (lambda (y) (= y x)) acc))) acc]
        [else (cons x acc)]))
    empty
    lst))

;; Question 2(e): zero-fill

(define (zero-fill str)
  (list->string
   (foldr cons
          (string->list str)
          (build-list
           (- 20 (length (string->list str)))
           (lambda (x) #\0)))))
