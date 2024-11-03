;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Q1

;; a)

(check-expect (my-list-ref (list 1 2 3 4) 1) 2)
(check-expect (my-list-ref (list 7 8 9) 0) 7)
(check-expect (my-list-ref (list 4) 3) false)

;; b)

(check-expect (zip (list "x" "y") (list 10 20)) (list (list "x" 10) (list "y" 20)))
(check-expect (zip (list) (list)) empty)
(check-expect (zip (list 3 6 9) (list "a" "b" "c")) (list (list 3 "a") (list 6 "b") (list 9 "c")))

;; c)

(check-expect (list-xor (list 1 4 7) (list 2 4 6)) (list 1 2 6 7))
(check-expect (list-xor (list) (list)) empty)
(check-expect (list-xor (list 5 10) (list 10 15)) (list 5 15))

;; Q2

;; b)

(check-expect (matrix-item (list (list 1 2 3 4) (list 5 6 7 8)) 1 2) 7)
(check-expect (matrix-item (list (list 10 20 30) (list 40 50 60)) 0 1) 20)
(check-expect (matrix-item (list (list 9 8 7) (list 6 5 4) (list 3 2 1)) 2 0) 3)

;; c)

(check-expect (matrix-col (list (list 1 2 3 4) (list 5 6 7 8)) 2) (list 3 7))
(check-expect (matrix-col (list (list 10 20 30) (list 40 50 60)) 0) (list 10 40))
(check-expect (matrix-col (list (list 9 8 7) (list 6 5 4) (list 3 2 1)) 1) (list 8 5 2))

;; d)

(check-expect (matrix-transpose (list (list 1 2 3 4) (list 5 6 7 8)))
              (list (list 1 5) (list 2 6) (list 3 7) (list 4 8)))
(check-expect (matrix-transpose empty) empty)
(check-expect (matrix-transpose (list (list 1 2))) (list (list 1) (list 2)))

;; e)

(check-expect (matrix-multiply (list (list 1 0) (list 0 1)) (list (list 1 2) (list 3 4)))
              (list (list 1 2) (list 3 4)))
(check-expect (matrix-multiply (list (list 2 3) (list 4 5)) (list (list 1 0) (list 0 1)))
              (list (list 2 3) (list 4 5)))
(check-expect (matrix-multiply (list (list 1 2)) (list (list 3) (list 4))) (list (list 11)))

;; Q3

;; a)

(check-expect (extreme-actions "Alice"
                               (list (list "Alice"
                                           (list (make-action -5 "Broke a window")
                                                 (make-action 10 "Helped a friend")))))
              (list "Broke a window" "Helped a friend"))
(check-expect (extreme-actions "Bob" (list (list "Alice" (list (make-action 10 "Helped a friend")))))
              empty)
(check-expect (extreme-actions "Zaphod"
                               (list (list "Zaphod"
                                           (list (make-action -7 "Questions are too hard")
                                                 (make-action 3 "Prepared assignment question")))))
              (list "Questions are too hard" "Prepared assignment question"))

;; b)

(check-expect (merge-actions (list (list "Zaphod"
                                         (list (make-action 3 "Prepared assignment question")
                                               (make-action -7 "Questions are too hard"))))
                             (list (list "Zaphod"
                                         (make-action 42 "Told a good joke about recursion."))))
              (list (list "Zaphod"
                          (list (make-action 42 "Told a good joke about recursion.")
                                (make-action 3 "Prepared assignment question")
                                (make-action -7 "Questions are too hard")))))
(check-expect
 (merge-actions (list) (list (list "Zaphod" (make-action 42 "Told a good joke about recursion."))))
 (list (list "Zaphod" (list (make-action 42 "Told a good joke about recursion.")))))
(check-expect
 (merge-actions (list (list "Alice" (list (make-action 5 "Did homework"))))
                (list (list "Alice" (make-action -10 "Skipped a lecture"))))
 (list (list "Alice" (list (make-action -10 "Skipped a lecture") (make-action 5 "Did homework")))))

;; c)

(check-expect (choose-gifts -10
                            (list (make-wish 32 "Amigurumi Bee Plushie")
                                  (make-wish 99 "Wayne Gretzky Rookie Card")))
              (list "coal"))
(check-expect (choose-gifts 50
                            (list (make-wish 32 "Amigurumi Bee Plushie")
                                  (make-wish 99 "Wayne Gretzky Rookie Card")))
              (list "Amigurumi Bee Plushie"))
(check-expect (choose-gifts 0
                            (list (make-wish 32 "Amigurumi Bee Plushie")
                                  (make-wish 99 "Wayne Gretzky Rookie Card")))
              (list "socks"))

;; d)

(check-expect (assign-gifts (list (list "Zaphod"
                                        (list (make-action -7 "Questions are too hard")
                                              (make-action 3 "Prepared assignment question"))))
                            (list (list "Zaphod"
                                        (list (make-wish 32 "Amigurumi Bee Plushie")
                                              (make-wish 99 "Wayne Gretzky Rookie Card")))))
              (list (list "Zaphod" (list "coal"))))
(check-expect
 (assign-gifts (list (list "Zaphod" (list (make-action 50 "Helped an old lady cross the street"))))
               (list (list "Zaphod"
                           (list (make-wish 32 "Amigurumi Bee Plushie")
                                 (make-wish 99 "Wayne Gretzky Rookie Card")))))
 (list (list "Zaphod" (list "Amigurumi Bee Plushie"))))
(check-expect (assign-gifts (list)
                            (list (list "Zaphod"
                                        (list (make-wish 32 "Amigurumi Bee Plushie")
                                              (make-wish 99 "Wayne Gretzky Rookie Card")))))
              (list (list "Zaphod" (list "socks"))))
