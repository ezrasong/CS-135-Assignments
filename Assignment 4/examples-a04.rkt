;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Q2

;; a)
(check-expect (list-has-exactly-4-symbols (list 'a 'b 'c 'd)) true)
(check-expect (list-has-exactly-4-symbols (list 'a 1 "b" 'c 'd)) false)

;; b)
(check-expect (add-only-numbers (list "text" 5 'sym 10)) 15)
(check-expect (add-only-numbers (list 'a 'b "str")) 0)

;; c)
(check-expect (before-after "cat" "hello" "world" (list "dog" "cat"))
              (list "dog" "hello" "cat" "world"))

;; d)
(check-expect (exists? 'apple (list 'orange 'banana 'apple)) true)
(check-expect (exists? 5 (list 1 2 3 4)) false)

;; e)
(check-expect (remove-duplicates (list 1 2 3 1 2 4)) (list 3 1 2 4))

;; Q3

;; a)
(check-expect (sorted? (list (make-card 2 'Diamond) (make-card 5 'Heart))) true)
(check-expect (sorted? (list (make-card 'King 'Club) (make-card 10 'Spade))) false)

;; b)
(check-expect (cheater? (list (make-card 2 'Diamond) (make-card 2 'Heart) (make-card 7 'Club))) false)

(check-expect (cheater? (list (make-card 'Ace 'Spade) (make-card 'Ace 'Spade))) true)

(check-expect (cheater? (list (make-card 2 'Diamond) (make-card 2 'Diamond) (make-card 3 'Club)))
              true)

;; c)
(check-expect (is-straight? (list (make-card 7 'Club)
                                  (make-card 8 'Spade)
                                  (make-card 9 'Heart)
                                  (make-card 10 'Diamond)
                                  (make-card 'Jack 'Club)))
              true)

;; d)
(check-expect (is-flush? (list (make-card 2 'Club)
                               (make-card 5 'Club)
                               (make-card 10 'Club)
                               (make-card 'Jack 'Club)
                               (make-card 'Ace 'Club)))
              true)

(check-expect (is-flush? (list (make-card 2 'Diamond)
                               (make-card 3 'Diamond)
                               (make-card 4 'Diamond)
                               (make-card 5 'Diamond)
                               (make-card 6 'Diamond)))
              true)

(check-expect (is-flush? (list (make-card 2 'Diamond)
                               (make-card 3 'Heart)
                               (make-card 4 'Club)
                               (make-card 5 'Spade)
                               (make-card 6 'Club)))
              false)

;; e)
(check-expect (is-full-house? (list (make-card 2 'Diamond)
                                    (make-card 2 'Heart)
                                    (make-card 2 'Spade)
                                    (make-card 'King 'Diamond)
                                    (make-card 'King 'Heart)))
              true)

(check-expect (is-full-house? (list (make-card 3 'Club)
                                    (make-card 3 'Diamond)
                                    (make-card 4 'Club)
                                    (make-card 5 'Spade)
                                    (make-card 6 'Club)))
              false)

;; f)
(check-expect (replace-card (make-card 2 'Diamond)
                            (make-card 'Ace 'Club)
                            (list (make-card 2 'Club)
                                  (make-card 2 'Diamond)
                                  (make-card 'Jack 'Club)
                                  (make-card 9 'Spade)
                                  (make-card 6 'Club)))
              (list (make-card 2 'Club)
                    (make-card 'Ace 'Club)
                    (make-card 'Jack 'Club)
                    (make-card 9 'Spade)
                    (make-card 6 'Club)))

(check-expect
 (replace-card (make-card 'King 'Heart)
               (make-card 5 'Diamond)
               (list (make-card 7 'Club) (make-card 'King 'Heart) (make-card 4 'Diamond)))
 (list (make-card 7 'Club) (make-card 5 'Diamond) (make-card 4 'Diamond)))
