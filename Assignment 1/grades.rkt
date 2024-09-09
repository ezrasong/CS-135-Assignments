;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 01, Question 5
;; ***************************************************

;; Q5 a) Function Definition for final-cs135-grade
(define (final-cs135-grade participation midterm final assignments)
  (+ (* participation 0.05) (* midterm 0.25) (* final 0.45) (* assignments 0.25)))

;; Q5 b) Function Definition for final-cs135-grade-needed
(define (cs135-final-exam-grade-needed participation midterm assignments)
  (/ (- 60 (* 0.05 participation) (* 0.25 midterm) (* 0.25 assignments)) 0.45))

