;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Q1

(check-expect
 (contains-component?
  (make-component "computer" 1 (list
    (make-component "motherboard" 1 (list
      (make-component "cpu" 1 empty)
      (make-component "ram" 2 empty)
      (make-component "gpu" 1 empty)))
    (make-component "case" 1 (list
      (make-component "power-supply" 1 empty)))))
  "cpu")
 true)

(check-expect
 (contains-component?
  (make-component "computer" 1 (list
    (make-component "motherboard" 1 (list
      (make-component "cpu" 1 empty)
      (make-component "ram" 2 empty)
      (make-component "gpu" 1 empty)))
    (make-component "case" 1 (list
      (make-component "power-supply" 1 empty)))))
  "hard-drive")
 false)

;; Q2

;; b)

(check-expect (eval (list 'XOR (list 1 1 0))) 0)
(check-expect (eval (list 'AND (list 0 1 1))) 0)
(check-expect (eval (list 'OR (list 1 0 0))) 1)

;; c)

(check-expect (bidexp->string (list 'AND (list 1 0 1))) "(t*f*t)")
(check-expect (bidexp->string (list 'XOR (list 1 0))) "(t.f)")
(check-expect (bidexp->string (list 'OR (list 1 1 1))) "(t+t+t)")

;; d)

(check-expect (eval-id (list 'AND (list 'a 'c)) (list (list 'a 1) (list 'b 0) (list 'c 1))) 1)
(check-expect (eval-id (list 'OR (list 'b 'c)) (list (list 'a 1) (list 'b 0) (list 'c 1))) 1)
(check-expect (eval-id (list 'XOR (list 'c 'b 'a)) (list (list 'a 1) (list 'b 0) (list 'c 1))) 0)

;; Q3

;; b)

(check-expect (create-tree (list "CAR" "CARD" "CARE" "CART" "CAT"))
  (make-node #\space false (list
    (make-node #\C false (list
      (make-node #\A false (list
        (make-node #\R true (list
          (make-node #\D true empty)
          (make-node #\E true empty)
          (make-node #\T true empty)))
        (make-node #\T true empty))))))))

;; c)

(check-expect (check "SOCCER" (create-tree (list "BASEBALL" "BASKETBALL" "HOCKEY" "SOCCER"))) true)
(check-expect (check "FOOTBALL" (create-tree (list "BASEBALL" "BASKETBALL" "HOCKEY" "SOCCER"))) false)
