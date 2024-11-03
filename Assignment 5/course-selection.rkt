;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname course-selection) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 05, Question 2
;; ***************************************************

;; Data Definitions:

;; A StudentEntry is a (list Str (listof Sym))
;; Interpretation: Represents a student's Quest username and their list of selected course codes.

;; A DesiredCourses is one of:
;; - empty
;; - (cons StudentEntry DesiredCourses)
;; Interpretation: A list of StudentEntry.

;; Examples:

(define selections
  (list
    (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
    (list "w46dles" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
    (list "d32pines" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
    (list "gnclstan" (list 'ANTH241 'LS201 'AMATH231 'PMATH347))))

;; Template for DesiredCourses:

;; (define (desiredcourses-template dc)
;;   (cond
;;     [(empty? dc) (...)]
;;     [else
;;       (... (first dc)           ; StudentEntry
;;            (first (first dc))   ; Str (username)
;;            (second (first dc))  ; (listof Sym) (courses)
;;            (desiredcourses-template (rest dc)))]))

;; Question 2(a):

;; missed-deadline-add: DesiredCourses Str -> DesiredCourses
;; Consumes a DesiredCourses and a student's Quest username.
;; If the username exists, returns DesiredCourses unchanged.
;; If not, adds a new StudentEntry with an empty course list at the end.

;; Examples:
(check-expect
 (missed-deadline-add selections "w2cordur")
 (list
   (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
   (list "w46dles" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
   (list "d32pines" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
   (list "gnclstan" (list 'ANTH241 'LS201 'AMATH231 'PMATH347))
   (list "w2cordur" empty)))

(define (missed-deadline-add dc username)
  ;; Checks if username exists and adds new entry if not.
  (cond
    [(username-exists? dc username) dc]
    [else (add-to-end dc (list username empty))]))

;; Helper Functions:

;; username-exists?: DesiredCourses Str -> Bool
;; Returns true if the username exists in DesiredCourses, false otherwise.
(define (username-exists? dc username)
  (cond
    [(empty? dc) false]
    [(string=? (first (first dc)) username) true]
    [else (username-exists? (rest dc) username)]))

;; add-to-end: (listof X) X -> (listof X)
;; Adds element x to the end of list lst.
(define (add-to-end lst x)
  (cond
    [(empty? lst) (cons x empty)]
    [else (cons (first lst) (add-to-end (rest lst) x))]))

;; Question 2(b):

;; taking-course?: DesiredCourses Str Sym -> Bool
;; Returns true if the student with the given username has selected the course code.
;; Returns false if the student does not exist or has not selected the course.

;; Examples:
(check-expect (taking-course? selections "d32pines" 'CS115) true)
(check-expect (taking-course? selections "d32pines" 'CS135) false)
(check-expect (taking-course? selections "nonexistent" 'CS115) false)

(define (taking-course? dc username course-code)
  (cond
    [(empty? dc) false]
    [(string=? (first (first dc)) username)
     (course-in-list? course-code (second (first dc)))]
    [else (taking-course? (rest dc) username course-code)]))

;; course-in-list?: Sym (listof Sym) -> Bool
;; Returns true if course-code is in course-list, false otherwise.
(define (course-in-list? course-code course-list)
  (cond
    [(empty? course-list) false]
    [(symbol=? (first course-list) course-code) true]
    [else (course-in-list? course-code (rest course-list))]))

;; Question 2(c):

;; add-course: DesiredCourses Str Sym -> DesiredCourses
;; Adds the course code to the student's course list.
;; If the student does not exist, adds a new StudentEntry with the course code.
;; If the student already has the course, returns DesiredCourses unchanged.

;; Examples:
(check-expect (add-course selections "gnclstan" 'CS246)
  (list
    (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
    (list "w46dles" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
    (list "d32pines" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
    (list "gnclstan" (list 'ANTH241 'LS201 'AMATH231 'PMATH347 'CS246))))
(check-expect (add-course empty "mdluffy" 'CS246)
  (list (list "mdluffy" (list 'CS246))))
(check-expect (add-course selections "d32pines" 'CS115) selections)

(define (add-course dc username course-code)
  (cond
    [(empty? dc)
     (cons (list username (list course-code)) empty)]
    [(string=? (first (first dc)) username)
     (cond
       [(course-in-list? course-code (second (first dc))) dc]
       [else
        (cons
         (list username (add-course-to-list (second (first dc)) course-code))
         (rest dc))])]
    [else
     (cons (first dc) (add-course (rest dc) username course-code))]))

;; add-course-to-list: (listof Sym) Sym -> (listof Sym)
;; Adds course-code to the end of course-list.
(define (add-course-to-list course-list course-code)
  (cond
    [(empty? course-list) (list course-code)]
    [else (cons (first course-list) (add-course-to-list (rest course-list) course-code))]))

;; Question 2(d):

;; create-classlist: DesiredCourses Sym -> (listof Str)
;; Produces a list of usernames of students who have selected the given course code.

;; Examples:
(check-expect (create-classlist selections 'MATH135) (list "mpines" "d32pines"))
(check-expect (create-classlist selections 'CS115) (list "d32pines"))
(check-expect (create-classlist selections 'CS246) empty)

(define (create-classlist dc course-code)
  (cond
    [(empty? dc) empty]
    [(course-in-list? course-code (second (first dc)))
     (cons (first (first dc)) (create-classlist (rest dc) course-code))]
    [else
     (create-classlist (rest dc) course-code)]))
