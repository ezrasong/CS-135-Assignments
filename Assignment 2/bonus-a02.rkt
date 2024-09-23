;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname roadtrip) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 02, Question 6
;; ***************************************************
(define (get-day date)
  (modulo date 100))

(define (get-month date)
  (modulo (quotient date 100) 100))

(define (get-year date)
  (quotient date 10000))

;; Adjusted Year: Year - floor((14 - Month) / 12)
(define (adjusted-year date)
  (- (get-year date) (floor (/ (- 14 (get-month date)) 12))))

;; Adjusted Month: Month + 12 * floor((14 - Month) / 12)
(define (adjusted-month date)
  (+ (get-month date) (* 12 (floor (/ (- 14 (get-month date)) 12)))))

(define (K date)
  (modulo (adjusted-year date) 100))

(define (J date)
  (quotient (adjusted-year date) 100))

(define (compute-h date)
  (modulo (+ (get-day date)
             (floor (/ (* 13 (+ (adjusted-month date) 1)) 5))
             (K date)
             (floor (/ (K date) 4))
             (floor (/ (J date) 4))
             (- (* 2 (J date))))
          7))

(define (h->day h)
  (cond
    [(= h 0) 'Saturday]
    [(= h 1) 'Sunday]
    [(= h 2) 'Monday]
    [(= h 3) 'Tuesday]
    [(= h 4) 'Wednesday]
    [(= h 5) 'Thursday]
    [(= h 6) 'Friday]))

(define (date->day-of-week date)
  (h->day (compute-h date)))

(check-expect (date->day-of-week 20240924) 'Tuesday)
(check-expect (date->day-of-week 38781202) 'Monday)
