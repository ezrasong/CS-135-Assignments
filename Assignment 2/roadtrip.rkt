;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname roadtrip) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 02, Question 5
;; ***************************************************

;; Helper Function to check fatigue status of driver
(define (status fatigue)
  (cond
    [(symbol=? fatigue 'exhausted) 'ready]
    [(symbol=? fatigue 'ready) 'rested]
    [(symbol=? fatigue 'rested) 'rested]
    [else 'invalid]))

;; Function to get the index of a city
(define (city-index city)
  (cond
    [(symbol=? city 'StJohns) 1]
    [(symbol=? city 'Charlottetown) 2]
    [(symbol=? city 'Halifax) 3]
    [(symbol=? city 'Fredericton) 4]
    [(symbol=? city 'QuebecCity) 5]
    [(symbol=? city 'Toronto) 6]
    [(symbol=? city 'Waterloo) 7]
    [(symbol=? city 'SaultSteMarie) 8]
    [(symbol=? city 'ThunderBay) 9]
    [(symbol=? city 'Winnipeg) 10]
    [(symbol=? city 'Regina) 11]
    [(symbol=? city 'Calgary) 12]
    [(symbol=? city 'Vancouver) 13]
    [else -1]))

;; Helper Function to check if two cities are next to each other
(define (city-next? from-city to-city)
 (= (+ 1 (city-index from-city)) (city-index to-city)))

;; Helper Function to check if two cities are two cities apart
(define (city-next2? from-city to-city)
  (= (+ 2 (city-index from-city)) (city-index to-city)))

;; Function to process a leg of the trip
(define (process-leg from-city to-city fatigue)
  (cond
    [(symbol=? from-city to-city) (status fatigue)]
    [(or (< (city-index to-city) (city-index from-city))
         (> (- (city-index to-city) (city-index from-city)) 2))
     'invalid]
    [(symbol=? fatigue 'exhausted) 'invalid]
    [(symbol=? fatigue 'ready)
     (cond
       [(city-next? from-city to-city) 'ready]
       [else 'invalid])]
    [(symbol=? fatigue 'rested)
     (cond
       [(city-next? from-city to-city) 'ready]
       [(city-next2? from-city to-city) 'exhausted]
       [else 'invalid])]
    [else 'invalid]))

;; Function to check the plan
(define (check-plan city1 city2 city3 city4)
  (process-step city1 city2 'rested city3 city4))

;; Helper Function to process a step of the trip
(define (process-step from-city to-city fatigue city3 city4)
  (cond
    [(symbol=? (process-leg from-city to-city fatigue) 'invalid) 'invalid]
    [else (process-step2 to-city city3 (process-leg from-city to-city fatigue) city4)]))

;; Helper Function to process a step of the trip
(define (process-step2 from-city to-city fatigue city4)
  (cond
    [(symbol=? (process-leg from-city to-city fatigue) 'invalid) 'invalid]
    [else (process-step3 to-city city4 (process-leg from-city to-city fatigue))]))

;; Helper Function to process a step of the trip
(define (process-step3 from-city to-city fatigue)
  (cond
    [(symbol=? (process-leg from-city to-city fatigue) 'invalid) 'invalid]
    [else (process-leg from-city to-city fatigue)]))

;; Tests
(check-expect (check-plan 'Halifax 'Fredericton 'Halifax 'Fredericton) 'invalid)
(check-expect (check-plan 'Waterloo 'Waterloo 'Waterloo 'Waterloo) 'rested)
(check-expect (check-plan 'Halifax 'QuebecCity 'QuebecCity 'Toronto) 'ready)
(check-expect (check-plan 'Halifax 'QuebecCity 'QuebecCity 'Waterloo) 'invalid)
