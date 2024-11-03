;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pizza-party) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 05, Question 4
;; ***************************************************

;; Data Definitions:

;; A PizzaType is one of:
;; - 'Hawaiian
;; - 'meaty
;; - 'veggie
;; Interpretation: The type of pizza.

;; A StudentChoice is a (list Str Sym Nat)
;; Interpretation: A student's name (Str), pizza type (Sym), and number of desired slices (Nat).

;; Examples of StudentChoice:
(define sc1 (list "Alice" 'Hawaiian 2))
(define sc2 (list "Bob" 'meaty 3))
(define sc3 (list "Charlie" 'Hawaiian 1))
(define sc4 (list "David" 'meaty 2))
(define sc5 (list "Eve" 'meaty 1))
(define sc6 (list "Frank" 'veggie 2))
(define sc7 (list "Grace" 'veggie 3))
(define sc8 (list "Heidi" 'Hawaiian 1))

;; A Section is a (list Str PosInt (listof StudentChoice))
;; Interpretation: Instructor's name (Str), section number (PosInt), and list of StudentChoices.

;; Examples of Section:
(define section1
  (list "Prof. Smith" 1 (list sc1 sc2 sc3 sc4 sc5)))

(define section2
  (list "Prof. Jones" 2 (list sc6 sc7 sc8)))

;; A Course is a (listof Section)
;; Interpretation: A list of Sections.

;; Example of Course:
(define course1 (list section1 section2))

;; Templates:

;; studentchoice-template: StudentChoice -> ...
(define (studentchoice-template sc)
  (... (first sc)    ;; Str: student's name
       (second sc)   ;; Sym: pizza type
       (third sc)))  ;; Nat: number of slices

;; studentchoices-template: (listof StudentChoice) -> ...
(define (studentchoices-template scs)
  (cond
    [(empty? scs) (...)]
    [else
      (... (studentchoice-template (first scs))
           (studentchoices-template (rest scs)))]))

;; section-template: Section -> ...
(define (section-template sec)
  (... (first sec)    ;; Str: instructor's name
       (second sec)   ;; PosInt: section number
       (third sec)    ;; (listof StudentChoice): list of student choices
       (studentchoices-template (third sec))))

;; course-template: Course -> ...
(define (course-template crs)
  (cond
    [(empty? crs) (...)]
    [else
      (... (section-template (first crs))
           (course-template (rest crs)))]))

;; Question 4(b):

;; popular-pizza: Section -> Sym
;; Consumes a Section and produces the symbol of the most popular pizza type.
;; Based on the number of students who choose that type (not slice count).
;; Assumes counts are distinct.

;; Examples:
(check-expect (popular-pizza section1) 'meaty)
(check-expect (popular-pizza section2) 'veggie)

(define (popular-pizza section)
  (max-pizza-type (count-pizza-types (third section))))

;; Helper Functions:

;; count-pizza-types: (listof StudentChoice) -> (list Nat Nat Nat)
;; Returns counts of students choosing 'Hawaiian', 'meaty', 'veggie' in that order.
(define (count-pizza-types scs)
  (cond
    [(empty? scs) (list 0 0 0)]
    [else
     (update-counts (second (first scs)) (count-pizza-types (rest scs)))]))

;; update-counts: Sym (list Nat Nat Nat) -> (list Nat Nat Nat)
;; Updates counts based on the pizza type.
(define (update-counts pizza-type counts)
  (cond
    [(symbol=? pizza-type 'Hawaiian)
     (list (+ 1 (first counts)) (second counts) (third counts))]
    [(symbol=? pizza-type 'meaty)
     (list (first counts) (+ 1 (second counts)) (third counts))]
    [(symbol=? pizza-type 'veggie)
     (list (first counts) (second counts) (+ 1 (third counts)))]))

;; max-pizza-type: (list Nat Nat Nat) -> Sym
;; Returns the pizza type with the highest count.
(define (max-pizza-type counts)
  (cond
    [(and (> (first counts) (second counts)) (> (first counts) (third counts))) 'Hawaiian]
    [(and (> (second counts) (first counts)) (> (second counts) (third counts))) 'meaty]
    [(and (> (third counts) (first counts)) (> (third counts) (second counts))) 'veggie]))

;; Question 4(c):

;; sort-choices: Section -> Section
;; Consumes a Section and produces the same Section with StudentChoices sorted.
;; Sorted by pizza type ('Hawaiian' < 'meaty' < 'veggie') and then by name alphabetically.

;; Examples:
(define sorted-section1
  (list "Prof. Smith" 1
    (list sc1 sc3 sc2 sc4 sc5)))

(check-expect (sort-choices section1) sorted-section1)

(define (sort-choices section)
  (list (first section)
        (second section)
        (insertion-sort (third section))))

;; choices<=: StudentChoice StudentChoice -> Bool
;; Returns true if sc1 should come before sc2 in the ordering.

(define (choices<= sc1 sc2)
  (cond
    [(< (pizza-rank (second sc1)) (pizza-rank (second sc2))) true]
    [(= (pizza-rank (second sc1)) (pizza-rank (second sc2)))
     (string<=? (first sc1) (first sc2))]
    [else false]))

;; pizza-rank: Sym -> Nat
;; Assigns a numerical rank to each pizza type.
(define (pizza-rank pizza-type)
  (cond
    [(symbol=? pizza-type 'Hawaiian) 1]
    [(symbol=? pizza-type 'meaty) 2]
    [(symbol=? pizza-type 'veggie) 3]))

;; Insertion Sort Functions:

;; insertion-sort: (listof StudentChoice) -> (listof StudentChoice)
(define (insertion-sort lst)
  (cond
    [(empty? lst) empty]
    [else (insert (first lst) (insertion-sort (rest lst)))]))

;; insert: StudentChoice (listof StudentChoice) -> (listof StudentChoice)
(define (insert x sorted-list)
  (cond
    [(empty? sorted-list) (cons x empty)]
    [(choices<= x (first sorted-list)) (cons x sorted-list)]
    [else (cons (first sorted-list) (insert x (rest sorted-list)))]))

;; Question 4(d):

;; pizza-lookup: Course PosInt Str -> (list Sym Nat)
;; Produces the pizza type and slice count for the given student in the specified section.

;; Examples:
(check-expect (pizza-lookup course1 1 "Bob") (list 'meaty 3))
(check-expect (pizza-lookup course1 2 "Grace") (list 'veggie 3))

(define (pizza-lookup crs section-number student-name)
  (pizza-lookup-in-students
    (third (find-section crs section-number))
    student-name))

;; find-section: Course PosInt -> Section
;; Finds the Section with the given section number.
(define (find-section crs section-number)
  (cond
    [(= (second (first crs)) section-number) (first crs)]
    [else (find-section (rest crs) section-number)]))

;; pizza-lookup-in-students: (listof StudentChoice) Str -> (list Sym Nat)
;; Finds the StudentChoice for the given student name.
(define (pizza-lookup-in-students scs student-name)
  (cond
    [(string=? (first (first scs)) student-name)
     (list (second (first scs)) (third (first scs)))]
    [else
     (pizza-lookup-in-students (rest scs) student-name)]))

;; Question 4(e):

;; count-slices: Course -> (list Nat Nat Nat)
;; Produces total slice counts for 'Hawaiian', 'meaty', 'veggie' pizzas in that order.

;; Example:
(check-expect (count-slices course1) (list 4 6 5))

(define (count-slices crs)
  (count-slices-course crs))

;; count-slices-course: Course -> (list Nat Nat Nat)
(define (count-slices-course crs)
  (cond
    [(empty? crs) (list 0 0 0)]
    [else
     (sum-counts (count-slices-section (first crs))
                 (count-slices-course (rest crs)))]))

;; count-slices-section: Section -> (list Nat Nat Nat)
(define (count-slices-section section)
  (count-slices-students (third section)))

;; count-slices-students: (listof StudentChoice) -> (list Nat Nat Nat)
(define (count-slices-students scs)
  (cond
    [(empty? scs) (list 0 0 0)]
    [else
     (sum-counts (count-slices-student (first scs))
                 (count-slices-students (rest scs)))]))

;; count-slices-student: StudentChoice -> (list Nat Nat Nat)
(define (count-slices-student sc)
  (cond
    [(symbol=? (second sc) 'Hawaiian) (list (third sc) 0 0)]
    [(symbol=? (second sc) 'meaty) (list 0 (third sc) 0)]
    [(symbol=? (second sc) 'veggie) (list 0 0 (third sc))]))

;; sum-counts: (list Nat Nat Nat) (list Nat Nat Nat) -> (list Nat Nat Nat)
;; Sums two counts lists.
(define (sum-counts counts1 counts2)
  (list (+ (first counts1) (first counts2))
        (+ (second counts1) (second counts2))
        (+ (third counts1) (third counts2))))
