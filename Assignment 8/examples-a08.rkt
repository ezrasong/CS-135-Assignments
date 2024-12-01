;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname s) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Q2

;; a)
(check-expect (or-pred positive? empty) false)
(check-expect (or-pred symbol? (list 1 'a 3)) true)
(check-expect (or-pred number? (list "hello" 'world 'test)) false)

;; b)
(check-expect (map2argfn (list + *) (list 3 5)) (list 8 15))
(check-expect (map2argfn (list expt quotient modulo) (list 5 3)) (list 125 1 2))
(check-expect (map2argfn (list (lambda (x y) (- x y)) (lambda (x y) (/ x y))) (list 10 2)) (list 8 5))

;; c)
(check-expect (arranged? (list number? <) empty) true)
(check-expect (arranged? (list positive? >) (list 9 7 5 3 1)) true)
(check-expect (arranged? (list symbol? string<?) (list 'apple "banana" 'cherry)) false)

;; Q3

(check-expect (partition even? (list 1 2 3 4 5 6)) (list (list 2 4 6) (list 1 3 5)))
(check-expect (partition symbol? (list 'a "b" 'c "d")) (list (list 'a 'c) (list "b" "d")))
(check-expect (partition positive? (list -3 -2 -1 0 1 2 3)) (list (list 1 2 3) (list -3 -2 -1 0)))

;; Q4

(check-expect ((tree-pred even?) (make-node 2 empty empty)) true)
(check-expect
 ((tree-pred (lambda (x) (< x 5))) (make-node 3 (make-node 2 empty empty) (make-node 4 empty empty)))
 true)
(check-expect
 ((tree-pred (lambda (x) (> x 0))) (make-node 5 (make-node 10 empty empty) (make-node 0 empty empty)))
 false)

;; Q5

;; b)
(check-expect (nested-filter number? '(1 "a" (2 "b" (3 "c")))) '(1 (2 (3))))
(check-expect (nested-filter positive? '(-2 (-1 0 1 2))) '((1 2)))
(check-expect (nested-filter even? '(1 2 (3 4 (5 6)))) '(2 (4 (6))))

;; c)
(check-expect (ruthless '(ruth apple ruth banana)) '(apple banana))
(check-expect (ruthless '((ruth) (orange ruth) grape)) '(() (orange) grape))
(check-expect (ruthless '(ruthless (ruth) ((ruth) ruthless) 'ruth)) '(ruthless () (() ruthless)))

;; d)
(check-expect (keep-between 0 5 '((-1 0 1) 2 (3 (4 5)) 6)) '((0 1) 2 (3 (4 5))))
(check-expect (keep-between -2 2 '((-3 -2 -1 0 1 2 3))) '((-2 -1 0 1 2)))
(check-expect (keep-between 10 20 '(5 (15 25 (20 10)))) '((15 (20 10))))

;; e)
(check-expect (nested-cleanup '((()) ((())))) false)
(check-expect (nested-cleanup '(a () (b ()))) '(a (b)))
(check-expect (nested-cleanup '(1 2 (3 (4 () 5)) () ())) '(1 2 (3 (4 5))))

;; f)
(check-expect (nested-apply (list sqrt floor) '(1.44 2.25 (3.61 4.84)))
              (list '(1.2 1.5 (1.9 2.2)) '(1 2 (3 4))))
(check-expect (nested-apply (list add1 sub1) '((0 -1) (2 3))) (list '((1 0) (3 4)) '((-1 -2) (1 2))))
(check-expect (nested-apply (list (lambda (x) (* x x)) (lambda (x) (* x 2))) '(1 2 (3 4)))
              (list '(1 4 (9 16)) '(2 4 (6 8))))
