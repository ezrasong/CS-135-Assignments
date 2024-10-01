;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 03, Question 4
;; ***************************************************

(define-struct sub-mod (name weight))
; A Sub-Mod has:
; - name: String
; - weight > 0

(define-struct mod (name frame-wt primary secondary))
; A Mod has:
; - name: String
; - frame-wt > 0
; - primary: Sub-Mod
; - secondary: Sub-Mod
;   (Note: primary.weight ≥ secondary.weight)

(define-struct robot (name head arms legs))
; A Robot has:
; - name: String
; - head: Mod
; - arms: Mod
; - legs: Mod

;; Make a function to produces the total weight of inputed module
;; which includes its frame as well as its two sub-modules.

;; Example:
(check-expect
 (mod-weight
  (make-mod "Riven-Arms" 5 (make-sub-mod "Riven-Forearm" 10) (make-sub-mod "Riven-Elbow" 7)))
 22)

(check-expect
 (mod-weight (make-mod "Ahri-Head" 0 (make-sub-mod "Ahri-Forehead" 10) (make-sub-mod "Ahri-Chin" 9)))
 19)

;; mod-weight: Mod -> Number
;; Calculates the total weight of a Mod, including its frame and sub-modules.
(define (mod-weight m)
  (+ (mod-frame-wt m)
     (sub-mod-weight (mod-primary m))
     (sub-mod-weight (mod-secondary m))))

;; (mod-weight m) produces the total weight of inputed module
;; which includes its frame as well as its two sub-modules.

;; Tests for mod-weight
(check-expect
 (mod-weight
  (make-mod "LeeSin-Legs" 20 (make-sub-mod "LeeSin-Thigh" 5) (make-sub-mod "LeeSin-Shin" 0)))
 25)

;; Define robot-weight helper function
;; robot-weight: Robot -> Num
;; Computes the total weight of a Robot.
(define (robot-weight r)
  (+ (mod-weight (robot-head r))
     (mod-weight (robot-arms r))
     (mod-weight (robot-legs r))))

;; Make a function predict-winner that consumes two Robots and produces 
;; the name of the heavier Robot of the two.

;; Example:
(check-expect
 (predict-winner
  (make-robot
   "Aatrox"
   (make-mod "Aatrox-Head" 10 (make-sub-mod "Aatrox-Forehead" 5) (make-sub-mod "Aatrox-Chin" 3))
   (make-mod "Aatrox-Arms" 0 (make-sub-mod "Aatrox-Forearm" 14) (make-sub-mod "Aatrox-Elbow" 7))
   (make-mod "Aatrox-Legs" 2 (make-sub-mod "Aatrox-Thigh" 1) (make-sub-mod "Aatrox-Shin" 0)))
  (make-robot "Zed"
              (make-mod "Zed-Head" 7 (make-sub-mod "Zed-Forehead" 3) (make-sub-mod "Zed-Chin" 3))
              (make-mod "Zed-Arms" 5 (make-sub-mod "Zed-Forearm" 8) (make-sub-mod "Zed-Elbow" 6))
              (make-mod "Zed-Legs" 7 (make-sub-mod "Zed-Thigh" 4) (make-sub-mod "Zed-Shin" 3))))
 "Zed")

;; predict-winner: Robot Robot -> String
;; Produces the name of the heavier Robot. If tied, produces either name.
(define (predict-winner r1 r2)
  (cond
    [(>= (robot-weight r1) (robot-weight r2)) (robot-name r1)]
    [else (robot-name r2)]))

;; (predict-winner r1 r2) consumes two Robots and produces 
;; the name of the heavier Robot of the two.

;; Tests for predict-winner
(check-expect
 (predict-winner
  (make-robot
   "Darius"
   (make-mod "Darius-Head" 14 (make-sub-mod "Darius-Forehead" 4) (make-sub-mod "Darius-Chin" 3))
   (make-mod "Darius-Arms" 5 (make-sub-mod "Darius-Forearm" 2) (make-sub-mod "Darius-Elbow" 2))
   (make-mod "Darius-Legs" 2 (make-sub-mod "Darius-Thigh" 2) (make-sub-mod "Darius-Shin" 0)))
  (make-robot
   "Garen"
   (make-mod "Garen-Head" 4 (make-sub-mod "Garen-Cheek" 2) (make-sub-mod "Garen-Mouth" 1))
   (make-mod "Garen-Arms" 8 (make-sub-mod "Garen-Forearm" 5) (make-sub-mod "Garen-Elbow" 2))
   (make-mod "Garen-Legs" 6 (make-sub-mod "Garen-Thigh" 1) (make-sub-mod "Garen-Shin" 1))))
 "Darius")

;; Helper function to get the heavier Mod based on frame-wt
;; heavier-mod: Mod Mod -> Mod
(define (heavier-mod m1 m2)
  (cond
    [(>= (mod-frame-wt m1) (mod-frame-wt m2)) m1]
    [else m2]))

;; Helper function to find the heaviest Sub-Mod among four
;; heaviest-submod: Sub-Mod Sub-Mod Sub-Mod Sub-Mod -> Sub-Mod
(define (heaviest-submod sm1 sm2 sm3 sm4)
  (cond
    [(and (>= (sub-mod-weight sm1) (sub-mod-weight sm2))
          (>= (sub-mod-weight sm1) (sub-mod-weight sm3))
          (>= (sub-mod-weight sm1) (sub-mod-weight sm4)))
     sm1]
    [(and (>= (sub-mod-weight sm2) (sub-mod-weight sm1))
          (>= (sub-mod-weight sm2) (sub-mod-weight sm3))
          (>= (sub-mod-weight sm2) (sub-mod-weight sm4)))
     sm2]
    [(and (>= (sub-mod-weight sm3) (sub-mod-weight sm1))
          (>= (sub-mod-weight sm3) (sub-mod-weight sm2))
          (>= (sub-mod-weight sm3) (sub-mod-weight sm4)))
     sm3]
    [else sm4]))

;; Helper function to find the second heaviest Sub-Mod among four, excluding the heaviest
;; second-heaviest-submod: Sub-Mod Sub-Mod Sub-Mod Sub-Mod Sub-Mod -> Sub-Mod
;; The first argument is the heaviest Sub-Mod to exclude
(define (second-heaviest-submod max sm1 sm2 sm3 sm4)
  (cond
    [(and (not (= (sub-mod-weight sm1) (sub-mod-weight max)))
          (>= (sub-mod-weight sm1) (sub-mod-weight sm2))
          (>= (sub-mod-weight sm1) (sub-mod-weight sm3))
          (>= (sub-mod-weight sm1) (sub-mod-weight sm4)))
     sm1]
    [(and (not (= (sub-mod-weight sm2) (sub-mod-weight max)))
          (>= (sub-mod-weight sm2) (sub-mod-weight sm1))
          (>= (sub-mod-weight sm2) (sub-mod-weight sm3))
          (>= (sub-mod-weight sm2) (sub-mod-weight sm4)))
     sm2]
    [(and (not (= (sub-mod-weight sm3) (sub-mod-weight max)))
          (>= (sub-mod-weight sm3) (sub-mod-weight sm1))
          (>= (sub-mod-weight sm3) (sub-mod-weight sm2))
          (>= (sub-mod-weight sm3) (sub-mod-weight sm4)))
     sm3]
    [else
     (cond
       [(not (= (sub-mod-weight sm4) (sub-mod-weight max))) sm4]
       [else sm1])]))

;; combine-modules: Mod Mod -> Mod
(define (combine-modules m1 m2)
  (make-mod
    ;; Name of the heavier frame-wt Mod
    (cond
      [(>= (mod-frame-wt m1) (mod-frame-wt m2)) (mod-name m1)]
      [else (mod-name m2)])
    ;; Heavier frame-wt
    (cond
      [(>= (mod-frame-wt m1) (mod-frame-wt m2)) (mod-frame-wt m1)]
      [else (mod-frame-wt m2)])
    ;; Primary Sub-Mod (heaviest among four)
    (heaviest-submod
      (mod-primary m1) (mod-secondary m1)
      (mod-primary m2) (mod-secondary m2))
    ;; Secondary Sub-Mod (second heaviest)
    (second-heaviest-submod
      (heaviest-submod
        (mod-primary m1) (mod-secondary m1)
        (mod-primary m2) (mod-secondary m2))
      (mod-primary m1) (mod-secondary m1)
      (mod-primary m2) (mod-secondary m2))))

;; Make a function that combines that consumes two Robots and 
;; produces a Robot according to the specified rules.

;; Example:
(check-expect
 (combine
  (make-robot
   "Ezreal"
   (make-mod "Ezreal-Head" 5 (make-sub-mod "Ezreal-Forehead" 8) (make-sub-mod "Ezreal-Chin" 5))
   (make-mod "Ezreal-Arms" 3 (make-sub-mod "Ezreal-Forearm" 13) (make-sub-mod "Ezreal-Elbow" 6))
   (make-mod "Ezreal-Legs" 8 (make-sub-mod "Ezreal-Thigh" 8) (make-sub-mod "Ezreal-Shin" 7)))
  (make-robot
   "Sivir"
   (make-mod "Sivir-Head" 4 (make-sub-mod "Sivir-Cheek" 7) (make-sub-mod "Sivir-Mouth" 4))
   (make-mod "Sivir-Arms" 30 (make-sub-mod "Sivir-Tricep" 9) (make-sub-mod "Sivir-Wrist" 8))
   (make-mod "Sivir-Legs" 9 (make-sub-mod "Sivir-Femur" 1) (make-sub-mod "Sivir-Ankle" 1))))
 (make-robot
  "Ezreal-Sivir"
  (make-mod "Ezreal-Head" 5 (make-sub-mod "Ezreal-Forehead" 8) (make-sub-mod "Sivir-Cheek" 7))
  (make-mod "Sivir-Arms" 30 (make-sub-mod "Ezreal-Forearm" 13) (make-sub-mod "Sivir-Tricep" 9))
  (make-mod "Sivir-Legs" 9 (make-sub-mod "Ezreal-Thigh" 8) (make-sub-mod "Ezreal-Shin" 7))))

;; combine: Robot Robot -> Robot
;; Produces a new Robot by combining the two given Robots according to the specified rules.
(define (combine r1 r2)
  (make-robot
    (string-append (robot-name r1) "-" (robot-name r2))
    (combine-modules (robot-head r1) (robot-head r2))
    (combine-modules (robot-arms r1) (robot-arms r2))
    (combine-modules (robot-legs r1) (robot-legs r2))))

;; (combine r1 r2) consumes two Robots and 
;; produces a combined Robot according to the specified rules.

;; Tests for combine
(check-expect
 (combine
  (make-robot
   "Yasuo"
   (make-mod "Yasuo-Head" 50 (make-sub-mod "Yasuo-Forehead" 60) (make-sub-mod "Yasuo-Chin" 20))
   (make-mod "Yasuo-Arms" 35 (make-sub-mod "Yasuo-Forearm" 55) (make-sub-mod "Yasuo-Elbow" 60))
   (make-mod "Yasuo-Legs" 30 (make-sub-mod "Yasuo-Thigh" 30) (make-sub-mod "Yasuo-Shin" 15)))
  (make-robot
   "Diana"
   (make-mod "Diana-Head" 30 (make-sub-mod "Diana-Cheek" 65) (make-sub-mod "Diana-Mouth" 40))
   (make-mod "Diana-Arms" 100 (make-sub-mod "Diana-Tricep" 80) (make-sub-mod "Diana-Wrist" 25))
   (make-mod "Diana-Legs" 70 (make-sub-mod "Diana-Femur" 45) (make-sub-mod "Diana-Ankle" 15))))
 (make-robot
  "Yasuo-Diana"
  (make-mod "Yasuo-Head" 50 (make-sub-mod "Diana-Cheek" 65) (make-sub-mod "Yasuo-Forehead" 60))
  (make-mod "Diana-Arms" 100 (make-sub-mod "Diana-Tricep" 80) (make-sub-mod "Yasuo-Elbow" 60))
  (make-mod "Diana-Legs" 70 (make-sub-mod "Diana-Femur" 45) (make-sub-mod "Yasuo-Thigh" 30))))