;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname assignment05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2024
;; Assignment 06, Question 3
;; ***************************************************

;; Data Definitions:

;; A Name is a Str
;; A Desc is a Str
;; A NiceScore n is an Int
;; Requires: -100 <= n <= 100 and n not= 0

(define-struct action (niceness desc))
;; An Action is a (make-action NiceScore Desc)

;; An ActionList is a (listof (list Name (listof Action)))
;; Requires: The list is sorted alphabetically by child name.
;; Each list of Actions is non-empty.
;; Note: the order of Actions for the same child is arbitrary.

;; extreme-actions:
;; Consumes a child's name and an ActionList and produces either:
;; - empty if the child's name does not appear in the ActionList.
;; - a list of two strings where the first string is the description of the child's action
;;   with the lowest niceness score and the second string is the description of the child's
;;   action with the highest niceness score.
;; If two actions have the same niceness score, choose the first one in the list.
;; If the child has only one recorded action, the extremes will be the same.

;; Examples:
(define action1 (make-action 3 "Prepared assignment question"))
(define action2 (make-action -7 "Questions are too hard"))
(define actlst (list (list "Zaphod" (list action1 action2))))

(check-expect (extreme-actions "Zaphod" actlst)
              (list "Questions are too hard" "Prepared assignment question"))

;; extreme-actions: Name ActionList -> (listof Desc)
(define (extreme-actions name alist)
  (cond
    [(empty? alist) empty]
    [(string=? (first (first alist)) name)
     (extreme-actions-helper (second (first alist)) (first (second (first alist)))
      (first (second (first alist))))]
    [else (extreme-actions name (rest alist))]))
;; (extreme-actions name alist) produces either an empty list if the child's name does not appear in 
;; alist, or a list containing two strings: the descriptions of the child's actions with the lowest 
;; and highest niceness scores in alist.

;; Helper Functions:

;; extreme-actions-helper: (listof Action) Action Action -> (listof Desc)
;; Consumes a list of Actions, current min-action, and current max-action,
;; and returns a list of two Descs: [min-desc max-desc].
(define (extreme-actions-helper actions min-action max-action)
  (cond
    [(empty? actions) (list (action-desc min-action) (action-desc max-action))]
    [else
     (extreme-actions-helper
      (rest actions)
      (update-min (first actions) min-action)
      (update-max (first actions) max-action))]))

;; update-min: Action Action -> Action
;; Returns the action with the lower niceness score.
(define (update-min current min)
  (cond
    [(< (action-niceness current) (action-niceness min)) current]
    [else min]))

;; update-max: Action Action -> Action
;; Returns the action with the higher niceness score.
(define (update-max current max)
  (cond
    [(> (action-niceness current) (action-niceness max)) current]
    [else max]))

;; merge-actions: 
;; Consumes an ActionList and an ActionUpdate, and produces a new ActionList
;; with any new children and actions included. The new ActionList is sorted alphabetically
;; by child name.

;; Examples:
(define action3 (make-action 42 "Told a good joke about recursion."))
(define newactlst (list (list "Zaphod" (list action3 action1 action2))))
(check-expect (merge-actions actlst (list (list "Zaphod" action3))) newactlst)

;; An ActionUpdate is a (listof (list Name Action))
;; Requires: The list is sorted alphabetically by child name.
;; A name will only appear once in the list.

;; merge-actions: ActionList ActionUpdate -> ActionList
(define (merge-actions alist aupdate)
  (cond
    [(and (empty? alist) (empty? aupdate)) empty]
    [(empty? alist) (convert-aupdate-to-alist aupdate)]
    [(empty? aupdate) alist]
    [(string<? (first (first alist)) (first (first aupdate)))
     (cons (first alist) (merge-actions (rest alist) aupdate))]
    [(string=? (first (first alist)) (first (first aupdate)))
     (cons (list (first (first alist))
                 (cons (second (first aupdate)) (second (first alist))))
           (merge-actions (rest alist) (rest aupdate)))]
    [else
     (cons (list (first (first aupdate)) (list (second (first aupdate))))
           (merge-actions alist (rest aupdate)))]))
;; (merge-actions alist aupdate) produces a new ActionList that includes any new children and actions 
;; from aupdate, sorted alphabetically by child name.

;; Helper Function:

;; convert-aupdate-to-alist: ActionUpdate -> ActionList
;; Converts an ActionUpdate to an ActionList.
(define (convert-aupdate-to-alist aupdate)
  (cond
    [(empty? aupdate) empty]
    [else
     (cons (list (first (first aupdate)) (list (second (first aupdate))))
           (convert-aupdate-to-alist (rest aupdate)))]))

;; choose-gifts:
;; Consumes an overall niceness score N and a (listof Wish), and produces a (listof Desc)
;; according to the specified rules.

;; A Wish is a (make-wish NiceScore Desc)
;; Requires: score is further restricted to be > 0
(define-struct wish (score gift))
;; A WishList is a (listof Wish)
;; Requires: Wishes are sorted in non-decreasing order by score.

;; Examples:
(define wish1 (make-wish 32 "Amigurumi Bee Plushie"))
(define wish2 (make-wish 99 "Wayne Gretzky Rookie Card"))
(define chldlst (list (list "Zaphod" (list wish1 wish2))))

(check-expect (choose-gifts -5 (list wish1 wish2)) (list "coal"))
(check-expect (choose-gifts 0 (list wish1 wish2)) (list "socks"))
(check-expect (choose-gifts 100 (list wish1 wish2))
              (list "Wayne Gretzky Rookie Card" "Amigurumi Bee Plushie"))

;; choose-gifts: Int (listof Wish) -> (listof Desc)
(define (choose-gifts N wishlist)
  (cond
    [(< N 0) (list "coal")]
    [(= N 0) (list "socks")]
    [(empty? wishlist) (list "socks")]
    [(< N (wish-score (first wishlist))) (list "socks")]
    [else (collect-gifts N wishlist empty)]))
;; (choose-gifts N wishlist) produces a list of gift descriptions based on the overall niceness score 
;; N and the wishlist. If N is negative, the child receives "coal"; if N is zero or there are no 
;; suitable gifts, they receive "socks." Otherwise, it includes gifts within the score limit N.

;; Helper Function:

;; collect-gifts: Int (listof Wish) (listof Desc) -> (listof Desc)
;; Collects gifts with score <= N, accumulating them in reverse order.
(define (collect-gifts N wishlist acc)
  (cond
    [(empty? wishlist) acc]
    [(<= (wish-score (first wishlist)) N)
     (collect-gifts N (rest wishlist) (cons (wish-gift (first wishlist)) acc))]
    [else acc]))

;; assign-gifts: ActionList ChildrenList -> GiftList
;; Consumes an ActionList and a ChildrenList and produces a GiftList.
;; A child's overall niceness score is the sum of their niceness scores in their actions.
;; If the child is not listed in the ActionList but is listed in the ChildrenList,
;; Santa assigns them a niceness score of 0.

;; A ChildrenList is a (listof (list Name Wishlist))
;; Requires: The list is sorted alphabetically by child name.
;; A GiftList is a (listof (list Name (listof Desc)))
;; Requires: The GiftList is sorted alphabetically by child name.
;; The gifts (listof Desc) are sorted in non-increasing order of score.

;; Examples:
(check-expect (assign-gifts actlst chldlst)
              (list (list "Zaphod" (list "coal"))))
(check-expect (assign-gifts newactlst chldlst)
              (list (list "Zaphod" (list "Amigurumi Bee Plushie"))))

(define (assign-gifts alist clist)
  (cond
    [(and (empty? alist) (empty? clist)) empty]
    [(empty? clist) empty]
    [(empty? alist)
     (cons (list (first (first clist))
                 (choose-gifts 0 (second (first clist))))
           (assign-gifts alist (rest clist)))]
    [(string<? (first (first alist)) (first (first clist)))
     (assign-gifts (rest alist) clist)]
    [(string=? (first (first alist)) (first (first clist)))
     (cons (list (first (first clist))
                 (choose-gifts (sum-niceness (second (first alist))) (second (first clist))))
           (assign-gifts (rest alist) (rest clist)))]
    [else
     (cons (list (first (first clist))
                 (choose-gifts 0 (second (first clist))))
           (assign-gifts alist (rest clist)))]))
;; (assign-gifts alist clist) produces a GiftList, where each child in clist receives a list of gift 
;; descriptions based on their overall niceness score from alist or a default score of 0 if they have 
;; no actions in alist. The GiftList is sorted alphabetically by child name.

;; Helper Function:

;; sum-niceness: (listof Action) -> Int
;; Sums the niceness scores of the actions.
(define (sum-niceness actions)
  (cond
    [(empty? actions) 0]
    [else
     (+ (action-niceness (first actions)) (sum-niceness (rest actions)))]))