;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 03, Bonus
;; ***************************************************

(define-struct box (xmin xmax ymin ymax))
;; A Box is a (make-box Num Num Num Num)

;; Make a function that returns the size of the overlapping region between two boxes

;; Examples
(define b1 (make-box 0 4 0 4))
(define b2 (make-box 2 6 2 6))
(check-expect (overlap-area b1 b2) 4)

(define b3 (make-box 5 10 5 10))
(check-expect (overlap-area b1 b3) 0)

(define b4 (make-box 3 7 1 5))
(check-expect (overlap-area b1 b4) 1)

(define b5 (make-box -2 2 -2 2))
(check-expect (overlap-area b1 b5) 4)

;; overlap-area: Box Box -> Num
(define (overlap-area box1 box2)
  ;; Calculates the overlapping area by computing the overlapping lengths in x and y dimensions
  (* (max 0 (- (min (box-xmax box1) (box-xmax box2))
               (max (box-xmin box1) (box-xmin box2))))
     (max 0 (- (min (box-ymax box1) (box-ymax box2))
               (max (box-ymin box1) (box-ymin box2))))))

;; (overlap-area box1 box2) returns the size of the overlapping region between box1 and box2.
;; If the boxes do not overlap, then 0 is returned.

;; Tests
(define b6 (make-box -3 -1 -3 -1))
(check-expect (overlap-area b1 b6) 0)

(define b7 (make-box 0 4 0 4))
(check-expect (overlap-area b1 b7) 16)

(define b8 (make-box 1 3 1 3))
(check-expect (overlap-area b1 b8) 4)

(define b9 (make-box 2 2 2 2))
(check-expect (overlap-area b1 b9) 0)