;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 04, Question 3
;; ***************************************************

(define-struct card (rank suit))

;; Function that checks if a non-empty list of Cards is sorted in increasing order.

;; Examples
(check-expect (sorted? (cons (make-card 2 'Diamond) (cons (make-card 7 'Club) empty))) true)

(check-expect (sorted? (cons (make-card 'Ace 'Spade) (cons (make-card 'Ace 'Club) empty))) false)

;; sorted?: (listof Card) -> Bool
(define (sorted? lst)
  (cond
    ;; If there's only one card, it's sorted
    [(empty? (rest lst)) true]
    ;; Otherwise, check if the first card is less than or equal to the next card
    [else (and (card-less-than-or-equal? (first lst) (first (rest lst))) (sorted? (rest lst)))]))

;; Helper function to compare two cards based on rank and suit
;; card-less-than-or-equal?: Card Card -> Bool
(define (card-less-than-or-equal? card1 card2)
  (cond
    ;; If rank of card1 is less than rank of card2, return true
    [(< (rank-value (card-rank card1)) (rank-value (card-rank card2))) true]
    ;; If ranks are equal, compare suits
    [(= (rank-value (card-rank card1)) (rank-value (card-rank card2)))
     (<= (suit-value (card-suit card1)) (suit-value (card-suit card2)))]
    ;; If rank of card1 is greater, return false
    [else false]))

;; Helper function to assign numerical values to ranks
;; rank-value: (anyof Int Sym) -> Num
(define (rank-value rank)
  (cond
    ;; For numbers 2-10, use the number itself
    [(number? rank) rank]
    ;; Assign values to face cards
    [(symbol=? rank 'Jack) 11]
    [(symbol=? rank 'Queen) 12]
    [(symbol=? rank 'King) 13]
    [(symbol=? rank 'Ace) 14]
    [else 0]))

;; Helper function to assign numerical values to suits
;; suit-value: Sym -> Num
(define (suit-value suit)
  (cond
    [(symbol=? suit 'Club) 1]
    [(symbol=? suit 'Diamond) 2]
    [(symbol=? suit 'Heart) 3]
    [(symbol=? suit 'Spade) 4]
    [else 0]))

;; (sorted? lst) returns true if the Cards in lst appear in increasing order, false otherwise.

;; Function that checks if a non-empty sorted list of Cards contains duplicate cards.

;; Examples
(check-expect (cheater? (cons (make-card 2 'Diamond)
                              (cons (make-card 2 'Heart) (cons (make-card 7 'Club) empty))))
              false)

(check-expect (cheater? (cons (make-card 'Ace 'Spade) (cons (make-card 'Ace 'Spade) empty))) true)

;; cheater?: (listof Card) -> Bool
(define (cheater? lst)
  (cond
    ;; If there's only one card, no duplicates
    [(empty? (rest lst)) false]
    ;; If the first two cards are the same, return true
    [else (or (cards-equal? (first lst) (first (rest lst))) (cheater? (rest lst)))]))

;; Helper function to compare two cards for equality
;; cards-equal?: Card Card -> Bool
(define (cards-equal? card1 card2)
  (and (equal-rank? (card-rank card1) (card-rank card2))
       (symbol=? (card-suit card1) (card-suit card2))))

;; Helper function to compare ranks for equality
;; equal-rank?: (anyof Int Sym) (anyof Int Sym) -> Bool
(define (equal-rank? rank1 rank2)
  (cond
    ;; If both are numbers, compare numerically
    [(and (number? rank1) (number? rank2)) (= rank1 rank2)]
    ;; If both are symbols, compare using symbol=?
    [(and (symbol? rank1) (symbol? rank2) (symbol=? rank1 rank2)) true]
    ;; Otherwise, ranks are not equal
    [else false]))

;; (cheater? lst) returns true if two cards in lst are exactly the same, false otherwise.

;; Function that checks if a sorted list of 5 Cards forms a straight.

;; Examples
(check-expect (is-straight? (cons (make-card 2 'Diamond)
                                  (cons (make-card 3 'Heart)
                                        (cons (make-card 4 'Club)
                                              (cons (make-card 5 'Spade)
                                                    (cons (make-card 6 'Club) empty))))))
              true)

(check-expect (is-straight? (cons (make-card 8 'Diamond)
                                  (cons (make-card 9 'Club)
                                        (cons (make-card 9 'Heart)
                                              (cons (make-card 'Queen 'Spade)
                                                    (cons (make-card 'King 'Club) empty))))))
              false)

;; is-straight?: (listof Card) -> Bool
(define (is-straight? lst)
  (cond
    ;; If there's only one card left, it's a straight
    [(empty? (rest lst)) true]
    ;; Check if the next rank is one higher than the current rank
    [else
     (and (= (+ (rank-value (card-rank (first lst))) 1) (rank-value (card-rank (first (rest lst)))))
          (is-straight? (rest lst)))]))

;; (is-straight? lst) returns true if the Cards in lst form a straight, false otherwise.

;; Function that checks if a sorted list of 5 Cards forms a flush.

;; Examples
(check-expect (is-flush? (cons (make-card 2 'Diamond)
                               (cons (make-card 3 'Heart)
                                     (cons (make-card 4 'Club)
                                           (cons (make-card 5 'Spade)
                                                 (cons (make-card 6 'Club) empty))))))
              false)

(check-expect (is-flush? (cons (make-card 2 'Club)
                               (cons (make-card 5 'Club)
                                     (cons (make-card 10 'Club)
                                           (cons (make-card 'Jack 'Club)
                                                 (cons (make-card 'Ace 'Club) empty))))))
              true)

;; is-flush?: (listof Card) -> Bool
(define (is-flush? lst)
  (cond
    ;; If there's only one card, it's a flush
    [(empty? (rest lst)) true]
    ;; Check if the suits of the first two cards are the same
    [else
     (and (symbol=? (card-suit (first lst)) (card-suit (first (rest lst)))) (is-flush? (rest lst)))]))

;; (is-flush? lst) returns true if all Cards in lst have the same suit, false otherwise.

;; Function that checks if a sorted list of 5 Cards forms a full house.

;; Examples
(check-expect (is-full-house? (cons (make-card 2 'Diamond)
                                    (cons (make-card 3 'Heart)
                                          (cons (make-card 4 'Club)
                                                (cons (make-card 5 'Spade)
                                                      (cons (make-card 6 'Club) empty))))))
              false)

(check-expect (is-full-house? (cons (make-card 3 'Club)
                                    (cons (make-card 3 'Diamond)
                                          (cons (make-card 'Jack 'Club)
                                                (cons (make-card 'Jack 'Heart)
                                                      (cons (make-card 'Jack 'Spade) empty))))))
              true)

;; is-full-house?: (listof Card) -> Bool
(define (is-full-house? lst)
  (or
   ;; Case 1: First two cards have the same rank, last three have the same rank, ranks are different
   (and (equal-rank? (card-rank (first lst)) (card-rank (first (rest lst))))
        (equal-rank? (card-rank (first (rest (rest lst))))
                     (card-rank (first (rest (rest (rest lst))))))
        (equal-rank? (card-rank (first (rest (rest (rest lst)))))
                     (card-rank (first (rest (rest (rest (rest lst)))))))
        (not (equal-rank? (card-rank (first lst)) (card-rank (first (rest (rest lst)))))))
   ;; Case 2: First three cards have the same rank, last two have the same rank, ranks are different
   (and (equal-rank? (card-rank (first lst)) (card-rank (first (rest lst))))
        (equal-rank? (card-rank (first (rest lst))) (card-rank (first (rest (rest lst)))))
        (equal-rank? (card-rank (first (rest (rest (rest lst)))))
                     (card-rank (first (rest (rest (rest (rest lst)))))))
        (not (equal-rank? (card-rank (first lst)) (card-rank (first (rest (rest (rest lst))))))))))

;; (is-full-house? lst) returns true if the Cards in lst form a full house, false otherwise.

;; Function that replaces occurrences of a specified card with another card in a list of Cards.

;; Example
(check-expect (replace-card (make-card 2 'Diamond)
                            (make-card 'Ace 'Club)
                            (cons (make-card 2 'Club)
                                  (cons (make-card 2 'Diamond)
                                        (cons (make-card 'Jack 'Club)
                                              (cons (make-card 9 'Spade)
                                                    (cons (make-card 6 'Club) empty))))))
              (cons (make-card 2 'Club)
                    (cons (make-card 'Ace 'Club)
                          (cons (make-card 'Jack 'Club)
                                (cons (make-card 9 'Spade) (cons (make-card 6 'Club) empty))))))

;; replace-card: Card Card (listof Card) -> (listof Card)
(define (replace-card old-card new-card lst)
  (cond
    ;; If the list is empty, return empty list
    [(empty? lst) empty]
    ;; If the first card matches old-card, replace it with new-card
    [(cards-equal? (first lst) old-card) (cons new-card (replace-card old-card new-card (rest lst)))]
    ;; Otherwise, keep the first card and recurse
    [else (cons (first lst) (replace-card old-card new-card (rest lst)))]))

;; (replace-card old-card new-card lst) returns a new list where every occurrence of old-card is
;; replaced with new-card.

;; Reusing cards-equal? and equal-rank? from previous problems.
