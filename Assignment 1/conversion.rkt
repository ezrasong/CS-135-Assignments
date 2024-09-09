;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 01, Question 4
;; ***************************************************

;; Q4 a) Function Definition for m/s->mph
(define (m/s->mph speed-m/s)
  (* speed-m/s (/ 3600 1609.344)))

;; Example test case
(check-expect (m/s->mph 1609.344) 3600)

;; Q4 b) Function Definition for mph->s/mfn
(define (mph->s/mfn mph)
  (/ (* (/ (* mph 1609.344) 3600.0) 1209.6) 1.7018))

;; Example test case
(check-expect (mph->s/mfn 1) (+ 317 (/ 1249 1675)))
