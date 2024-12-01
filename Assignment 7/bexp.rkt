;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname assignment05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Ezra Song
;; CS 135 Fall 2023
;; Assignment 07, Question 2
;; ***************************************************

;; Data Definitions:

;; A Boolean is one of:
;; - 0
;; - 1

;; A BExp (Boolean Expression) is one of:
;; - Boolean
;; - OpNode

;; An OpNode is a (list Sym (listof BExp))
;; where Sym is one of 'AND, 'OR, 'XOR
;; Interpretation: An operation node representing a boolean operation on a list of BExps.

;; Function Templates:

;; bexp-template: BExp -> ...
(define (bexp-template bexp)
  (cond
    [(number? bexp) ...]  ; Boolean
    [(list? bexp)
     (cond
       [else (opnode-template bexp)])]))

;; opnode-template: OpNode -> ...
(define (opnode-template opnode)
  (... (first opnode)         ; operator
       (operands-template (second opnode))
       ...))

;; operands-template: (listof BExp) -> ...
(define (operands-template operands)
  (cond
    [(empty? operands) ...]
    [else
     (... (bexp-template (first operands))
          (operands-template (rest operands))
          ...)]))

;; For our marking engine to find your function templates, they must be named bexp-template and opnode-template.

;; (b)

;; eval:
;; Evaluates the given Boolean Expression and produces a Boolean result.

;; Examples:
(check-expect (eval (list 'AND (list 0 1 1))) 0)
(check-expect (eval (list 'OR (list 0 1 1))) 1)
(check-expect (eval (list 'XOR (list 0 1 1))) 0)
(check-expect (eval (list 'AND (list 1 (list 'XOR (list 0 1 (list 'OR (list 0 0 0)))) 1))) 1)

;; eval: BExp -> Boolean
(define (eval bexp)
  (cond
    [(number? bexp) bexp]
    [(list? bexp)
     (eval-list-bexp bexp)]
    [else (error "Invalid BExp" bexp)]))
;; (eval bexp) evaluates a Boolean Expression (BExp) and produces 
;; a Boolean result (0 for false, 1 for true).

;; eval-list-bexp: (list Any Any) -> Boolean
;; Helper function to evaluate a list-based BExp.
(define (eval-list-bexp bexp)
  (cond
    [(= (length bexp) 2) (eval-list-bexp-2 bexp)]
    [else (error "Invalid BExp" bexp)]))

(define (eval-list-bexp-2 bexp)
  (cond
    [(symbol? (first bexp)) (eval-list-bexp-3 bexp)]
    [else (error "Invalid BExp" bexp)]))

(define (eval-list-bexp-3 bexp)
  (cond
    [(list? (second bexp)) (eval-opnode bexp)]
    [else (error "Invalid BExp" bexp)]))

;; eval-opnode: OpNode -> Boolean
;; Evaluates the OpNode and produces a Boolean result.
(define (eval-opnode opnode)
  (cond
    [(symbol=? (first opnode) 'AND) (eval-and (second opnode))]
    [(symbol=? (first opnode) 'OR) (eval-or (second opnode))]
    [(symbol=? (first opnode) 'XOR) (eval-xor (second opnode))]
    [else (error "Unknown operator" (first opnode))]))
;; (eval-opnode opnode) evaluates an OpNode based on its operator 
;; (AND, OR, or XOR) and produces a Boolean result.

;; eval-and: (listof BExp) -> Boolean
;; Evaluates the AND operation with short-circuiting.
(define (eval-and bexps)
  (cond
    [(empty? bexps) 1]
    [(= (eval (first bexps)) 0) 0]
    [else (eval-and (rest bexps))]))
;; (eval-and bexps) evaluates an AND operation with short-circuiting, 
;; returning 0 if any operand is 0 and 1 otherwise.

;; eval-or: (listof BExp) -> Boolean
;; Evaluates the OR operation with short-circuiting.
(define (eval-or bexps)
  (cond
    [(empty? bexps) 0]
    [(= (eval (first bexps)) 1) 1]
    [else (eval-or (rest bexps))]))
;; (eval-or bexps) evaluates an OR operation with short-circuiting, 
;; returning 1 if any operand is 1 and 0 otherwise.

;; eval-xor: (listof BExp) -> Boolean
;; Evaluates the XOR operation.
(define (eval-xor bexps)
  (eval-xor-helper bexps 0))
;; (eval-xor bexps) evaluates an XOR operation on a list of BExps, 
;; returning 1 if there is an odd number of 1s and 0 if even.

;; eval-xor-helper: (listof BExp) Int -> Boolean
;; Helper function for eval-xor that accumulates the result.
(define (eval-xor-helper bexps acc)
  (cond
    [(empty? bexps) acc]
    [else
     (eval-xor-helper (rest bexps)
                      (modulo (+ acc (eval (first bexps))) 2))]))

;; (c)

;; Updating data definitions to include identifiers:

;; A BIDExp (Boolean Id Expression) is one of:
;; - Boolean
;; - Sym
;; - OpNode

;; bidexp->string:
;; Converts the BIDExp into its string representation in infix notation.

;; Examples:
(check-expect (bidexp->string (list 'AND (list 0 1 1))) "(f*t*t)")
(check-expect (bidexp->string (list 'OR (list 0 1 1))) "(f+t+t)")
(check-expect (bidexp->string (list 'XOR (list 0 1 1))) "(f.t.t)")
(check-expect (bidexp->string (list 'AND (list 1 (list 'XOR (list 0 1 (list 'OR (list 0 0 0)))) 1))) "(t*(f.t.(f+f+f))*t)")
(check-expect (bidexp->string (list 'AND (list 1 (list 'AND (list 0 1))))) "(t*(f*t))")
(check-expect (bidexp->string (list 'XOR (list 0 't 'u 1 'w))) "(f.'t.'u.t.'w)")

;; bidexp->string: BIDExp -> Str
(define (bidexp->string exp)
  (cond
    [(number? exp)
     (cond
       [(= exp 1) "t"]
       [else "f"])]
    [(symbol? exp)
     (string-append "'" (symbol->string exp))]
    [(list? exp)
     (bidexp-list-exp->string exp)]
    [else (error "Invalid BIDExp" exp)]))
;; (bidexp->string exp) converts a Boolean Id Expression (BIDExp) to its string representation 
;; in infix notation, using symbols *, +, and . for AND, OR, and XOR 
;; respectively, with "t" and "f" for 1 and 0.

;; bidexp-list-exp->string: (list Any Any) -> Str
;; Helper function to process list-based BIDExp.
(define (bidexp-list-exp->string exp)
  (cond
    [(= (length exp) 2) (bidexp-list-exp-2 exp)]
    [else (error "Invalid BIDExp" exp)]))

(define (bidexp-list-exp-2 exp)
  (cond
    [(symbol? (first exp)) (bidexp-list-exp-3 exp)]
    [else (error "Invalid BIDExp" exp)]))

(define (bidexp-list-exp-3 exp)
  (cond
    [(list? (second exp)) (bidexp-opnode->string exp)]
    [else (error "Invalid BIDExp" exp)]))

;; bidexp-opnode->string: OpNode -> Str
;; Helper function to convert OpNode to string.
(define (bidexp-opnode->string opnode)
  (string-append "("
                 (bidexp-operands->string (second opnode) (operator->string (first opnode)))
                 ")"))
;; (bidexp-opnode->string opnode) converts an OpNode to its string representation with infix notation.

;; bidexp-operands->string: (listof BIDExp) Str -> Str
;; Converts the list of operands into a string with the given operator.
(define (bidexp-operands->string operands op-str)
  (cond
    [(empty? operands) ""]
    [else
     (string-append (bidexp->string (first operands))
                    (bidexp-operands-helper (rest operands) op-str))]))

;; bidexp-operands-helper: (listof BIDExp) Str -> Str
;; Helper function to concatenate operand strings.
(define (bidexp-operands-helper operands op-str)
  (cond
    [(empty? operands) ""]
    [else
     (string-append op-str
                    (bidexp->string (first operands))
                    (bidexp-operands-helper (rest operands) op-str))]))

;; operator->string: Sym -> Str
;; Converts operator symbols to their string representations.
(define (operator->string op)
  (cond
    [(symbol=? op 'AND) "*"]
    [(symbol=? op 'OR) "+"]
    [(symbol=? op 'XOR) "."]
    [else (error "Unknown operator" op)]))
;; (operator->string op) converts an operator symbol ('AND, 'OR, 'XOR) 
;; into its string representation (*, +, .).

;; (d)

;; eval-id:
;; Evaluates the BIDExp using the identifier table.

;; Examples:
(define identifier-table (list (list 'x 1) (list 'y 0)))
(check-expect (eval-id (list 'AND (list 0 'x 1)) identifier-table) 0)
(check-expect (eval-id (list 'OR (list 'x 'y 1)) identifier-table) 1)
(check-expect (eval-id (list 'XOR (list 0 'y 1)) identifier-table) 1)

;; eval-id: BIDExp (listof (list Sym Boolean)) -> Boolean
(define (eval-id exp id-table)
  (cond
    [(number? exp) exp]
    [(symbol? exp) (lookup exp id-table)]
    [(list? exp)
     (eval-id-list-exp exp id-table)]
    [else (error "Invalid BIDExp" exp)]))
;; (eval-id exp id-table) evaluates a BIDExp using an identifier table (id-table) that 
;; maps symbols to Boolean values.

;; eval-id-list-exp: (list Any Any) (listof (list Sym Boolean)) -> Boolean
;; Helper function to process list-based BIDExp.
(define (eval-id-list-exp exp id-table)
  (cond
    [(= (length exp) 2) (eval-id-list-exp-2 exp id-table)]
    [else (error "Invalid BIDExp" exp)]))

(define (eval-id-list-exp-2 exp id-table)
  (cond
    [(symbol? (first exp)) (eval-id-list-exp-3 exp id-table)]
    [else (error "Invalid BIDExp" exp)]))

(define (eval-id-list-exp-3 exp id-table)
  (cond
    [(list? (second exp)) (eval-id-opnode exp id-table)]
    [else (error "Invalid BIDExp" exp)]))

;; eval-id-opnode: OpNode (listof (list Sym Boolean)) -> Boolean
;; Evaluates the OpNode using the identifier table.
(define (eval-id-opnode opnode id-table)
  (cond
    [(symbol=? (first opnode) 'AND) (eval-id-and (second opnode) id-table)]
    [(symbol=? (first opnode) 'OR) (eval-id-or (second opnode) id-table)]
    [(symbol=? (first opnode) 'XOR) (eval-id-xor (second opnode) id-table)]
    [else (error "Unknown operator" (first opnode))]))
;; (eval-opnode opnode) evaluates an OpNode based on its operator (AND, OR, or XOR)
;; and produces a Boolean result.

;; eval-id-and: (listof BIDExp) (listof (list Sym Boolean)) -> Boolean
;; Evaluates the AND operation with short-circuiting using the identifier table.
(define (eval-id-and bexps id-table)
  (cond
    [(empty? bexps) 1]
    [(= (eval-id (first bexps) id-table) 0) 0]
    [else (eval-id-and (rest bexps) id-table)]))
;; (eval-and bexps) evaluates an AND operation with short-circuiting, returning 0 
;; if any operand is 0 and 1 otherwise.

;; eval-id-or: (listof BIDExp) (listof (list Sym Boolean)) -> Boolean
;; Evaluates the OR operation with short-circuiting using the identifier table.
(define (eval-id-or bexps id-table)
  (cond
    [(empty? bexps) 0]
    [(= (eval-id (first bexps) id-table) 1) 1]
    [else (eval-id-or (rest bexps) id-table)]))
;; (eval-or bexps) evaluates an OR operation with short-circuiting, 
;; returning 1 if any operand is 1 and 0 otherwise.

;; eval-id-xor: (listof BIDExp) (listof (list Sym Boolean)) -> Boolean
;; Evaluates the XOR operation using the identifier table.
(define (eval-id-xor bexps id-table)
  (eval-id-xor-helper bexps 0 id-table))
;; (eval-xor bexps) evaluates an XOR operation on a list of 
;; BExps, returning 1 if there is an odd number of 1s and 0 if even.

;; eval-id-xor-helper: (listof BIDExp) Int (listof (list Sym Boolean)) -> Boolean
;; Helper function for eval-id-xor that accumulates the result.
(define (eval-id-xor-helper bexps acc id-table)
  (cond
    [(empty? bexps) acc]
    [else
     (eval-id-xor-helper (rest bexps)
                         (modulo (+ acc (eval-id (first bexps) id-table)) 2)
                         id-table)]))

;; lookup: Sym (listof (list Sym Boolean)) -> Boolean
;; Looks up the value of a symbol in the identifier table.
(define (lookup x id-table)
  (cond
    [(empty? id-table) (error 'lookup "No binding for" x)]
    [(symbol=? x (first (first id-table))) (second (first id-table))]
    [else (lookup x (rest id-table))]))
;; (lookup x id-table) retrieves the Boolean value for the symbol 
;; x from the identifier table id-table, producing an error if x is not found.