;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 02, Question 4
;; ***************************************************

;; Function that determines if a donor can donate blood to a recipient only using cond and not and or
(define (can-donate-to/cond? donor recipient)
  (cond
    ;; O- is the universal donor
    [(symbol=? donor 'O-) #t]

    ;; O+ can donate to O+, A+, B+, and AB+
    [(symbol=? donor 'O+)
     (cond
       [(symbol=? recipient 'O+) true]
       [(symbol=? recipient 'A+) true]
       [(symbol=? recipient 'B+) true]
       [(symbol=? recipient 'AB+) true]
       [else false])]

    ;; A- can donate to A-, A+, AB-, and AB+
    [(symbol=? donor 'A-)
     (cond
       [(symbol=? recipient 'A-) true]
       [(symbol=? recipient 'A+) true]
       [(symbol=? recipient 'AB-) true]
       [(symbol=? recipient 'AB+) true]
       [else false])]

    ;; A+ can donate to A+ and AB+
    [(symbol=? donor 'A+)
     (cond
       [(symbol=? recipient 'A+) true]
       [(symbol=? recipient 'AB+) true]
       [else false])]

    ;; B- can donate to B-, B+, AB-, and AB+
    [(symbol=? donor 'B-)
     (cond
       [(symbol=? recipient 'B-) true]
       [(symbol=? recipient 'B+) true]
       [(symbol=? recipient 'AB-) true]
       [(symbol=? recipient 'AB+) true]
       [else false])]

    ;; B+ can donate to B+ and AB+
    [(symbol=? donor 'B+)
     (cond
       [(symbol=? recipient 'B+) true]
       [(symbol=? recipient 'AB+) true]
       [else false])]

    ;; AB- can donate to AB- and AB+
    [(symbol=? donor 'AB-)
     (cond
       [(symbol=? recipient 'AB-) true]
       [(symbol=? recipient 'AB+) true]
       [else false])]

    ;; AB+ can only donate to AB+
    [(symbol=? donor 'AB+) (symbol=? recipient 'AB+)]

    ;; Default case for invalid input
    [else false]))

;; Testing the function using check-expect
(check-expect (can-donate-to/cond? 'O- 'O-) true)
(check-expect (can-donate-to/cond? 'A+ 'B-) false)
(check-expect (can-donate-to/cond? 'B+ 'AB+) true)
(check-expect (can-donate-to/cond? 'AB- 'AB+) true)

;; Function that determines if a donor can donate blood to a recipient
(define (can-donate-to/bool? donor recipient)
  ;; O- can donate to anyone
  (or (symbol=? donor 'O-)
      ;; O+ can donate to O+, A+, B+, and AB+
      (and (symbol=? donor 'O+)
           (or (symbol=? recipient 'O+)
               (symbol=? recipient 'A+)
               (symbol=? recipient 'B+)
               (symbol=? recipient 'AB+)))
      ;; A- can donate to A-, A+, AB-, and AB+
      (and (symbol=? donor 'A-)
           (or (symbol=? recipient 'A-)
               (symbol=? recipient 'A+)
               (symbol=? recipient 'AB-)
               (symbol=? recipient 'AB+)))
      ;; A+ can donate to A+ and AB+
      (and (symbol=? donor 'A+) (or (symbol=? recipient 'A+) (symbol=? recipient 'AB+)))
      ;; B- can donate to B-, B+, AB-, and AB+
      (and (symbol=? donor 'B-)
           (or (symbol=? recipient 'B-)
               (symbol=? recipient 'B+)
               (symbol=? recipient 'AB-)
               (symbol=? recipient 'AB+)))
      ;; B+ can donate to B+ and AB+
      (and (symbol=? donor 'B+) (or (symbol=? recipient 'B+) (symbol=? recipient 'AB+)))
      ;; AB- can donate to AB- and AB+
      (and (symbol=? donor 'AB-) (or (symbol=? recipient 'AB-) (symbol=? recipient 'AB+)))
      ;; AB+ can donate only to AB+
      (and (symbol=? donor 'AB+) (symbol=? recipient 'AB+))))

;; Testing the function using check-expect
(check-expect (can-donate-to/bool? 'O- 'O-) true)
(check-expect (can-donate-to/bool? 'A+ 'B-) false)
(check-expect (can-donate-to/bool? 'B+ 'AB+) true)
(check-expect (can-donate-to/bool? 'AB- 'AB+) true)
