;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Q2

;; a)

(check-expect (missed-deadline-add (list (list "mpines" (list 'CS135 'MATH135))
                                         (list "w46dles" (list 'ARBUS101 'ECON101)))
                                   "mpines")
              (list (list "mpines" (list 'CS135 'MATH135))
                    (list "w46dles" (list 'ARBUS101 'ECON101))))

;; b)

(check-expect
 (taking-course? (list (list "d32pines" (list 'CS115 'MATH135 'ENGL109))) "d32pines" 'CS115)
 true)

;; c)

(check-expect
 (add-course (list (list "gnclstan" (list 'ANTH241 'LS201 'PMATH347))) "gnclstan" 'PMATH347)
 (list (list "gnclstan" (list 'ANTH241 'LS201 'PMATH347))))

;; d)

(check-expect
 (create-classlist (list (list "mpines" (list 'CS135 'MATH135 'MATH137 'ENGL109 'FINE100))
                         (list "w46dles" (list 'ARBUS101 'ECON101 'ECON102 'ECON206 'LS101))
                         (list "d32pines" (list 'CS115 'MATH135 'MATH137 'ENGL109 'FINE100))
                         (list "gnclstan" (list 'ANTH241 'LS201 'AMATH231 'PMATH347 'CS246)))
                   'MATH135)
 (list "mpines" "d32pines"))

;; Q3

(check-expect (make-symbol-lists (list 2 1 3) 'X) (list (list 'X 'X) (list 'X) (list 'X 'X 'X)))
(check-expect (make-symbol-lists empty 'Y) empty)
(check-expect (make-symbol-lists (list 3 0 2) 'Z) (list (list 'Z 'Z 'Z) empty (list 'Z 'Z)))
(check-expect (make-symbol-lists (list 3 0 2) 'Z) (list (list 'Z 'Z 'Z) empty (list 'Z 'Z)))

;; Q4

;; b)

(check-expect (popular-pizza (list "Prof Jones"
                                   2
                                   (list (list "Frank" 'Hawaiian 1)
                                         (list "Grace" 'veggie 2)
                                         (list "Heidi" 'veggie 1)
                                         (list "Ivan" 'meaty 2))))
              'veggie)

;; c)

(check-expect (sort-choices (list "Prof Smith"
                                  1
                                  (list (list "Charlie" 'veggie 1)
                                        (list "Bob" 'meaty 2)
                                        (list "Alice" 'Hawaiian 3)
                                        (list "Dana" 'veggie 2)
                                        (list "Eve" 'meaty 2))))
              (list "Prof Smith"
                    1
                    (list (list "Alice" 'Hawaiian 3)
                          (list "Bob" 'meaty 2)
                          (list "Eve" 'meaty 2)
                          (list "Charlie" 'veggie 1)
                          (list "Dana" 'veggie 2))))

;; d)

(check-expect
 (pizza-lookup
  (list (list "Prof Smith"
              1
              (list (list "Alice" 'Hawaiian 3) (list "Bob" 'meaty 2) (list "Charlie" 'veggie 1)))
        (list "Prof Jones"
              2
              (list (list "Dana" 'veggie 2) (list "Eve" 'meaty 2) (list "Frank" 'Hawaiian 1))))
  2
  "Frank")
 (list 'Hawaiian 1))

;; e)

(check-expect
 (count-slices
  (list (list "Prof Smith"
              1
              (list (list "Alice" 'Hawaiian 3) (list "Bob" 'meaty 2) (list "Charlie" 'veggie 1)))
        (list "Prof Jones"
              2
              (list (list "Dana" 'veggie 2) (list "Eve" 'meaty 2) (list "Frank" 'Hawaiian 1)))))
 (list 4 4 3))
