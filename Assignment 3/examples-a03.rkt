;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 03, Question 2
;; ***************************************************
;; Testing point-mult
;; 2a)
(check-expect (point-mult (make-point 1 2) (make-point 3 4)) (make-point -5 10))
(check-expect (point-mult (make-point 0 0) (make-point 5 7)) (make-point 0 0))

;; Testing point-div
;; 2b)
(check-expect (point-div (make-point 2 1) (make-point 1 2)) (make-point 4/5 -3/5))
(check-expect (point-div (make-point 4 2) (make-point 2 2)) (make-point 3/2 -1/2))

;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 03, Question 3
;; ***************************************************
;; Testing turn-left command
(check-expect (robot-ctl (make-state 5 5 'North) 'turn-left) (make-state 5 5 'West))
(check-expect (robot-ctl (make-state 5 5 'West) 'turn-left) (make-state 5 5 'South))
(check-expect (robot-ctl (make-state 5 5 'South) 'turn-left) (make-state 5 5 'East))
(check-expect (robot-ctl (make-state 5 5 'East) 'turn-left) (make-state 5 5 'North))

;; Testing turn-right command
(check-expect (robot-ctl (make-state 5 5 'North) 'turn-right) (make-state 5 5 'East))
(check-expect (robot-ctl (make-state 5 5 'East) 'turn-right) (make-state 5 5 'South))
(check-expect (robot-ctl (make-state 5 5 'South) 'turn-right) (make-state 5 5 'West))
(check-expect (robot-ctl (make-state 5 5 'West) 'turn-right) (make-state 5 5 'North))

;; Testing forward command
(check-expect (robot-ctl (make-state 5 5 'North) 'forward) (make-state 5 6 'North))
(check-expect (robot-ctl (make-state 5 5 'South) 'forward) (make-state 5 4 'South))
(check-expect (robot-ctl (make-state 5 5 'East) 'forward) (make-state 6 5 'East))
(check-expect (robot-ctl (make-state 5 5 'West) 'forward) (make-state 4 5 'West))

;; Testing boundary conditions
(check-expect (robot-ctl (make-state 10 10 'North) 'forward) (make-state 10 10 'North))
(check-expect (robot-ctl (make-state 0 0 'South) 'forward) (make-state 0 0 'South))
(check-expect (robot-ctl (make-state 10 5 'East) 'forward) (make-state 10 5 'East))
(check-expect (robot-ctl (make-state 0 5 'West) 'forward) (make-state 0 5 'West))

;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 03, Question 4
;; ***************************************************

;; 4b)
(check-expect
 (mod-weight
  (make-mod "Riven-Arms" 5 (make-sub-mod "Riven-Forearm" 10) (make-sub-mod "Riven-Elbow" 7)))
 22)

(check-expect
 (mod-weight (make-mod "Ahri-Head" 0 (make-sub-mod "Ahri-Forehead" 10) (make-sub-mod "Ahri-Chin" 9)))
 19)

(check-expect
 (mod-weight
  (make-mod "LeeSin-Legs" 20 (make-sub-mod "LeeSin-Thigh" 5) (make-sub-mod "LeeSin-Shin" 0)))
 25)

;; 4c)

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

(check-expect
 (predict-winner
  (make-robot
   "Leona"
   (make-mod "Leona-Head" 12 (make-sub-mod "Leona-Forehead" 4) (make-sub-mod "Leona-Chin" 3))
   (make-mod "Leona-Arms" 5 (make-sub-mod "Leona-Forearm" 2) (make-sub-mod "Leona-Elbow" 2))
   (make-mod "Leona-Legs" 2 (make-sub-mod "Leona-Thigh" 2) (make-sub-mod "Leona-Shin" 0)))
  (make-robot "Sona"
              (make-mod "Sona-Head" 4 (make-sub-mod "Sona-Cheek" 2) (make-sub-mod "Sona-Mouth" 1))
              (make-mod "Sona-Arms" 8 (make-sub-mod "Sona-Forearm" 5) (make-sub-mod "Sona-Elbow" 2))
              (make-mod "Sona-Legs" 6 (make-sub-mod "Sona-Thigh" 1) (make-sub-mod "Sona-Shin" 1))))
 "Leona")

;; 4d)

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

(check-expect
 (combine
  (make-robot "Lux"
              (make-mod "Lux-Head" 8 (make-sub-mod "Lux-Forehead" 7) (make-sub-mod "Lux-Chin" 3))
              (make-mod "Lux-Arms" 5 (make-sub-mod "Lux-Forearm" 9) (make-sub-mod "Lux-Elbow" 3))
              (make-mod "Lux-Legs" 10 (make-sub-mod "Lux-Thigh" 4) (make-sub-mod "Lux-Shin" 3)))
  (make-robot
   "Orianna"
   (make-mod "Orianna-Head" 13 (make-sub-mod "Orianna-Cheek" 8) (make-sub-mod "Orianna-Mouth" 2))
   (make-mod "Orianna-Arms" 6 (make-sub-mod "Orianna-Tricep" 2) (make-sub-mod "Orianna-Wrist" 1))
   (make-mod "Orianna-Legs" 9 (make-sub-mod "Orianna-Femur" 10) (make-sub-mod "Orianna-Ankle" 8))))
 (make-robot
  "Lux-Orianna"
  (make-mod "Orianna-Head" 13 (make-sub-mod "Orianna-Cheek" 8) (make-sub-mod "Lux-Forehead" 7))
  (make-mod "Orianna-Arms" 6 (make-sub-mod "Lux-Forearm" 9) (make-sub-mod "Lux-Elbow" 3))
  (make-mod "Lux-Legs" 10 (make-sub-mod "Orianna-Femur" 10) (make-sub-mod "Orianna-Ankle" 8))))
