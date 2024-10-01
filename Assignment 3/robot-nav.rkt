;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 03, Question 2
;; ***************************************************

;; Data definition for robot State

;; Define the State structure
(define-struct state (x y dir))
;; A State is a (make-state x y dir)
;; where:
;; - x is an Integer between 0 and 10 inclusive,
;; - y is an Integer between 0 and 10 inclusive,
;; - dir is one of the Symbols 'North, 'South, 'East, or 'West.

;; Make function that takes a direction and returns the new direction after turning left

;; Examples for turn-left
(check-expect (robot-ctl (make-state 5 5 'North) 'turn-left) (make-state 5 5 'West))
(check-expect (robot-ctl (make-state 5 5 'West) 'turn-left) (make-state 5 5 'South))
(check-expect (robot-ctl (make-state 5 5 'South) 'turn-left) (make-state 5 5 'East))
(check-expect (robot-ctl (make-state 5 5 'East) 'turn-left) (make-state 5 5 'North))

;; turn-left: Symbol -> Symbol
;; (turn-left dir) returns the new direction after turning left from dir
(define (turn-left dir)
  (cond
    [(symbol=? dir 'North) 'West]
    [(symbol=? dir 'West) 'South]
    [(symbol=? dir 'South) 'East]
    [(symbol=? dir 'East) 'North]
    [else dir]))

;; Tests for turn-left
(check-expect (turn-left 'North) 'West)
(check-expect (turn-left 'West) 'South)
(check-expect (turn-left 'South) 'East)
(check-expect (turn-left 'East) 'North)

;; Make function that takes a direction and returns the new direction after turning right

;; Examples for turn-right
(check-expect (robot-ctl (make-state 5 5 'North) 'turn-right) (make-state 5 5 'East))
(check-expect (robot-ctl (make-state 5 5 'East) 'turn-right) (make-state 5 5 'South))
(check-expect (robot-ctl (make-state 5 5 'South) 'turn-right) (make-state 5 5 'West))
(check-expect (robot-ctl (make-state 5 5 'West) 'turn-right) (make-state 5 5 'North))

;; turn-right: Symbol -> Symbol
;; (turn-right dir) returns the new direction after turning right from dir
(define (turn-right dir)
  (cond
    [(symbol=? dir 'North) 'East]
    [(symbol=? dir 'East) 'South]
    [(symbol=? dir 'South) 'West]
    [(symbol=? dir 'West) 'North]
    [else dir]))

;; Tests for turn-right
(check-expect (turn-right 'North) 'East)
(check-expect (turn-right 'East) 'South)
(check-expect (turn-right 'South) 'West)
(check-expect (turn-right 'West) 'North)

;; Make a function that consumes a State and returns the new state after moving forward

;; Examples for move-forward
(check-expect (robot-ctl (make-state 5 5 'North) 'forward) (make-state 5 6 'North))
(check-expect (robot-ctl (make-state 5 5 'South) 'forward) (make-state 5 4 'South))
(check-expect (robot-ctl (make-state 5 5 'East) 'forward) (make-state 6 5 'East))
(check-expect (robot-ctl (make-state 5 5 'West) 'forward) (make-state 4 5 'West))

;; move-forward: State -> State
;; (move-forward st) returns the new state after moving forward from st
(define (move-forward st)
  (cond
    [(and (symbol=? (state-dir st) 'North) (< (state-y st) 10))
     (make-state (state-x st) (+ (state-y st) 1) (state-dir st))]
    [(and (symbol=? (state-dir st) 'South) (> (state-y st) 0))
     (make-state (state-x st) (- (state-y st) 1) (state-dir st))]
    [(and (symbol=? (state-dir st) 'East) (< (state-x st) 10))
     (make-state (+ (state-x st) 1) (state-y st) (state-dir st))]
    [(and (symbol=? (state-dir st) 'West) (> (state-x st) 0))
     (make-state (- (state-x st) 1) (state-y st) (state-dir st))]
    [else st]))

;; Tests for move-forward
(define s6 (make-state 5 5 'North))
(check-expect (move-forward s6) (make-state 5 6 'North))

(define s7 (make-state 5 10 'North))
(check-expect (move-forward s7) (make-state 5 10 'North))

(define s8 (make-state 0 0 'South))
(check-expect (move-forward s8) (make-state 0 0 'South))

(define s9 (make-state 0 0 'East))
(check-expect (move-forward s9) (make-state 1 0 'East))

;;  Make a function that consumes a State and a command. Commands are the symbols
;; 'forward, 'turn-left, and 'turn-right. robot-ctl produces a new State

;; Examples for robot-ctl
(check-expect (robot-ctl (make-state 10 10 'North) 'forward) (make-state 10 10 'North))
(check-expect (robot-ctl (make-state 0 0 'South) 'forward) (make-state 0 0 'South))
(check-expect (robot-ctl (make-state 10 5 'East) 'forward) (make-state 10 5 'East))
(check-expect (robot-ctl (make-state 0 5 'West) 'forward) (make-state 0 5 'West))

;; robot-ctl: State Symbol -> State
;; (robot-ctl st cmd) produces a new state after applying cmd to st
(define (robot-ctl st cmd)
  (cond
    [(symbol=? cmd 'turn-left) (make-state (state-x st) (state-y st) (turn-left (state-dir st)))]
    [(symbol=? cmd 'turn-right) (make-state (state-x st) (state-y st) (turn-right (state-dir st)))]
    [(symbol=? cmd 'forward) (move-forward st)]
    [else st]))

;; (robot-ctl) consumes a State and a command. Commands are the symbols
;; 'forward, 'turn-left, and 'turn-right. robot-ctl produces a new State


;; Tests for robot-ctl
(define s1 (make-state 5 5 'North))
(check-expect (robot-ctl s1 'turn-left) (make-state 5 5 'West))
(check-expect (robot-ctl s1 'turn-right) (make-state 5 5 'East))
(check-expect (robot-ctl s1 'forward) (make-state 5 6 'North))

(define s2 (make-state 0 0 'West))
(check-expect (robot-ctl s2 'forward) (make-state 0 0 'West))
