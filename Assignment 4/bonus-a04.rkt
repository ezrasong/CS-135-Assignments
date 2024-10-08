;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song (21136337)
;; CS 135 Fall 2024
;; Assignment 04, Question 4
;; ***************************************************

;; Function that compares a secret word and a guess word, assigning 'green', 
;; 'yellow', or 'gray' to each letter.

;; Example
(check-expect (wordle-guess (cons #\h (cons #\e (cons #\l (cons #\l (cons #\o empty)))))
                            (cons #\s (cons #\t (cons #\o (cons #\l (cons #\e empty))))))
              (cons 'gray (cons 'gray (cons 'yellow (cons 'green (cons 'yellow empty))))))

;; wordle-guess: (listof Char) (listof Char) -> (listof Sym)
(define (wordle-guess secret guess)
  (cond
    ;; If the secret word is empty, return empty list
    [(empty? secret) empty]
    ;; Otherwise, compare the first letters and recurse on the rest
    [else
     (cons (check-letter (first secret) (first guess) secret)
           (wordle-guess (rest secret) (rest guess)))]))

;; Helper function to check a single letter
;; check-letter: Char Char (listof Char) -> Sym
(define (check-letter s-char g-char secret)
  (cond
    ;; If the letters are the same, return 'green
    [(char=? s-char g-char) 'green]
    ;; If the guess letter is in the secret word, return 'yellow
    [(char-in-list? g-char secret) 'yellow]
    ;; Otherwise, return 'gray
    [else 'gray]))

;; Helper function to check if a character is in a list of characters
;; char-in-list?: Char (listof Char) -> Bool
(define (char-in-list? c lst)
  (cond
    ;; If the list is empty, return false
    [(empty? lst) false]
    ;; If the first character matches, return true
    [(char=? c (first lst)) true]
    ;; Otherwise, recurse on the rest of the list
    [else (char-in-list? c (rest lst))]))

;; (wordle-guess secret guess) returns a list of symbols ('green, 'yellow, or 'gray)
;; corresponding to the comparison between the secret word and the guess word.
