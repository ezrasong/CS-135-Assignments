;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname n-queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 10, Question 4
;; ***************************************************

;; A Position is a (list Nat Nat)
;;
;; A Candidate is a (listof Position)
;; Requires: No two Positions are the same

;; (a) attacking?

;; Examples:
(check-expect (attacking? '(0 0) '(0 1)) true)    ; Same row
(check-expect (attacking? '(0 0) '(1 0)) true)    ; Same column
(check-expect (attacking? '(0 0) '(1 1)) true)    ; Same diagonal
(check-expect (attacking? '(0 0) '(2 1)) false)   ; Not attacking
(check-expect (attacking? '(1 2) '(3 0)) true)    ; Same diagonal

;; attacking?: Position Position -> Bool
;; (attacking? p1 p2) determines if two queens at positions p1 and p2 attack 
;; each other based on chess rules.
(define (attacking? p1 p2)
  ;; Determines if two queens at positions p1 and p2 are attacking each other.
  (cond
    [(= (first p1) (first p2)) true]   ; Same row
    [(= (second p1) (second p2)) true] ; Same column
    [(= (abs (- (first p1) (first p2)))
        (abs (- (second p1) (second p2)))) true] ; Same diagonal
    [else false]))
;; (attacking? p1 p2) determines if two queens at positions p1 and p2 attack
;; each other based on chess rules:
;; They attack if they are in the same row, column, or diagonal.

;; (b) valid-cand?

;; Examples:
(check-expect (valid-cand? '((0 0)) 4) true)
(check-expect (valid-cand? '((0 0) (1 2)) 4) true)
(check-expect (valid-cand? '((0 0) (1 0)) 4) false) ; Same column
(check-expect (valid-cand? '((0 0) (1 1)) 4) false) ; Attacking diagonally
(check-expect (valid-cand? '((0 0) (4 1)) 4) false) ; Out of bounds

;; valid-cand?: Candidate Nat -> Bool
;; (valid-cand? cand n) checks if a candidate configuration cand is valid for an N-Queens puzzle 
;; of size n. A candidate is valid if:
;; - All positions are within bounds (0 ≤ row, col < n).
;; - No two queens attack each other.
(define (valid-cand? cand n)
  ;; Checks if cand is a valid candidate for an N-Queens puzzle of size n.
  (local
    [
      ;; in-bounds?: Position -> Bool
      (define (in-bounds? pos)
        (and (<= 0 (first pos)) (< (first pos) n)
             (<= 0 (second pos)) (< (second pos) n)))

      ;; positions-valid?: (listof Position) -> Bool
      (define (positions-valid? positions)
        (cond
          [(empty? positions) true]
          [else
           (and (in-bounds? (first positions))
                (no-attack-with-others? (first positions) (rest positions))
                (positions-valid? (rest positions)))]))

      ;; no-attack-with-others?: Position (listof Position) -> Bool
      (define (no-attack-with-others? pos others)
        (cond
          [(empty? others) true]
          [else
           (and (not (attacking? pos (first others)))
                (no-attack-with-others? pos (rest others)))]))
    ]
    (positions-valid? cand)))
;; (valid-cand? cand n) checks if a candidate configuration cand is valid for an N-Queens puzzle of 
;; size n. A candidate is valid if:
;; All positions are within bounds (0 ≤ row, col < n).
;; No two queens attack each other.

;; (c) neighbours-naive

;; Examples:
(check-expect (neighbours-naive '((0 0)) 4)
              '(((1 2) (0 0)) ((1 3) (0 0)) ((2 1) (0 0))
                ((2 3) (0 0)) ((3 1) (0 0)) ((3 2) (0 0))))

;; neighbours-naive: Candidate Nat -> (listof Candidate)
;; (neighbours-naive cand n) generates a list of all possible neighbor configurations by 
;; adding a queen to any unoccupied and non-attacking square on the board.
(define (neighbours-naive cand n)
  ;; Generates neighbours by adding one queen to any unoccupied, non-attacking square.
  (local
    [
      ;; position-not-in-cand?: Position Candidate -> Bool
      (define (position-not-in-cand? pos cand)
        (cond
          [(empty? cand) true]
          [else
           (and (not (equal? pos (first cand)))
                (position-not-in-cand? pos (rest cand)))]))

      ;; not-attacking-cand?: Position Candidate -> Bool
      (define (not-attacking-cand? pos cand)
        (cond
          [(empty? cand) true]
          [else
           (and (not (attacking? pos (first cand)))
                (not-attacking-cand? pos (rest cand)))]))

      ;; generate-positions: Nat -> (listof Position)
      (define (generate-positions n)
        (generate-positions-row 0 n))

      ;; generate-positions-row: Nat Nat -> (listof Position)
      (define (generate-positions-row row n)
        (cond
          [(>= row n) empty]
          [else
           (append (generate-positions-col row 0 n)
                   (generate-positions-row (+ row 1) n))]))

      ;; generate-positions-col: Nat Nat Nat -> (listof Position)
      (define (generate-positions-col row col n)
        (cond
          [(>= col n) empty]
          [else
           (cons (list row col)
                 (generate-positions-col row (+ col 1) n))]))

      ;; filter-valid-positions: (listof Position) -> (listof Candidate)
      (define (filter-valid-positions positions)
        (cond
          [(empty? positions) empty]
          [(and (position-not-in-cand? (first positions) cand)
                (not-attacking-cand? (first positions) cand))
           (cons (cons (first positions) cand)
                 (filter-valid-positions (rest positions)))]
          [else
           (filter-valid-positions (rest positions))]))
    ]
    (filter-valid-positions (generate-positions n))))
;; (neighbours-naive cand n) generates a list of all possible "neighbor" 
;; configurations by adding a queen to any unoccupied and non-attacking square on the board.

;; (d) neighbours-row

;; Examples:
(check-expect (neighbours-row '((0 0)) 4)
              '(((1 2) (0 0)) ((1 3) (0 0))))

;; neighbours-row: Candidate Nat -> (listof Candidate)
;; (neighbours-row cand n) generates a list of neighbor configurations by adding a queen
;; to the next unoccupied row, ensuring no two queens are placed in the same row.
(define (neighbours-row cand n)
  ;; Generates neighbours by adding one queen in the next unoccupied row.
  (local
    [
      ;; position-not-in-cand?: Position Candidate -> Bool
      (define (position-not-in-cand? pos cand)
        (cond
          [(empty? cand) true]
          [else
           (and (not (equal? pos (first cand)))
                (position-not-in-cand? pos (rest cand)))]))

      ;; not-attacking-cand?: Position Candidate -> Bool
      (define (not-attacking-cand? pos cand)
        (cond
          [(empty? cand) true]
          [else
           (and (not (attacking? pos (first cand)))
                (not-attacking-cand? pos (rest cand)))]))

      ;; next-row: Candidate -> Nat
      (define (next-row cand)
        (cond
          [(empty? cand) 0]
          [else (+ (first (first cand)) 1)]))

      ;; generate-row-positions: Nat Nat -> (listof Position)
      (define (generate-row-positions row n)
        (generate-positions-col row 0 n))

      ;; generate-positions-col: Nat Nat Nat -> (listof Position)
      (define (generate-positions-col row col n)
        (cond
          [(>= col n) empty]
          [else
           (cons (list row col)
                 (generate-positions-col row (+ col 1) n))]))

      ;; filter-valid-positions: (listof Position) -> (listof Candidate)
      (define (filter-valid-positions positions)
        (cond
          [(empty? positions) empty]
          [(and (position-not-in-cand? (first positions) cand)
                (not-attacking-cand? (first positions) cand))
           (cons (cons (first positions) cand)
                 (filter-valid-positions (rest positions)))]
          [else
           (filter-valid-positions (rest positions))]))
    ]
    (filter-valid-positions (generate-row-positions (next-row cand) n))))
;; (neighbours-row cand n) generates a list of neighbor configurations by adding a 
;; queen to the next unoccupied row, ensuring no two queens are placed in the same row.

;; (e) n-queens

;; Examples:
(check-expect (n-queens 4 neighbours-naive)
              (list (list (list 3 1) (list 2 3) (list 1 0) (list 0 2))
                    (list (list 3 2) (list 2 0) (list 1 3) (list 0 1))))

;; n-queens: Nat ((Candidate Nat -> (listof Candidate))) -> (listof Candidate)
;; (n-queens n nbr-fun) solves the N-Queens problem for a board of size n using a neighbor generation 
;; function nbr-fun. It:
;; - Searches recursively through all valid candidate configurations.
;; - Uses the neighbor function (neighbours-naive or neighbours-row) to 
;;   generate extensions of a candidate.
(define (n-queens n nbr-fun)
  ;; Produces all full solutions to the N-Queens problem of size n.
  (local
    [
      ;; search: Candidate -> (listof Candidate)
      (define (search cand)
        (cond
          [(and (= (length cand) n) (valid-cand? cand n))
           (list cand)]
          [(= (length cand) n) empty]
          [else
           (search-list (nbr-fun cand n))]))

      ;; search-list: (listof Candidate) -> (listof Candidate)
      (define (search-list cands)
        (cond
          [(empty? cands) empty]
          [else
           (append (search (first cands))
                   (search-list (rest cands)))]))
    ]
    (search empty)))
;; (n-queens n nbr-fun) solves the N-Queens problem for a board of size n using a neighbor generation 
;; function nbr-fun. It:
;; Searches recursively through all valid candidate configurations.
;; Uses the neighbor function (neighbours-naive or neighbours-row) to 
;; generate extensions of a candidate.