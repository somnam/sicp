;; Helpers
(define (say . input) (display input) (newline))

;; 3.3 Modeling with Mutable Data
;; Data structures are specified in terms of constructors, which create data
;; objects and selectors, which access parts of compound data. In order to model
;; systems composed of objects with changing state, we need to modify them as
;; well as construct and select from them. We will design data abstractions
;; to include operations called mutators, which modify data objects.
;; Data objects for which mutators are defined are known as mutable data objects.

;; 3.3.1 Mutable list structure.
;; We will define basic mutators for pairs, so that pairs can serve as building
;; blocks for constructing mutable data objects.
;; Basic operations on pairs - cons, car, cdr are incapable of modifying list
;; structure. The same is true of the list operations we have used so far. To
;; modify list structures we need new operations.
;; The primitive mutators for pairs are set-car! and set-cdr!. THe first one 
;; takes two arguments, first of which must be a pair. It replaces the car
;; pointer of this pair by the second argument of set-car!.
;; x and y are bound to given lists:
(define x (list (list 'a 'b) 'c 'd))
(say "x:" x)
(define y (list 'e 'f))
(say "y:" y)
;; Evaluating the expression (set-car! x y) modifies the pair to which x is 
;; bound, replacing it's car with the value of y:
(set-car! x y) (say "set-car! x y" x)
;; The pairs representing the list (a b) are now detached from the original
;; structure. 
;; The set-cdr! operation replaces the cdr pointer of the pair:
(set-cdr! x y) (say "set-cdr! x y" x)
;; The list (c d) is now detached from the structure.

;; We could implement cons operation in terms of two mutators, together with
;; a procedure get-new-pair which terurns a new pair that is not part of any
;; existing list structure.
(define (get-new-pair) (cons '() '()))
(define (cons2 x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))
(say "cons" (cons2 1 2))

;; Exercise 3.12
;; The procedure append! is a mutator, that appends two lists by splicing them
;; together. It modifies the final pair of x so that its cdr is now y.
(define (last pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (append! x y)
  (set-cdr! (last-pair x) y))
(define x (list 'a 'b 'c 'd))
(define y (list 'e 'f))
(append! x y)
(say "append!" x)

;; Exercise 3.13
;; Consider the following make-cycle procedure:
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))
(say "make-cycle" z)

;; Exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
(define v (list 'a 'b 'c 'd))
(say "mystery" (mystery v))
;; The mystery is a 'reverse' function.

;; Sharing an identity.
;; Consider the structure formed by:
(define x (list 'a 'b))
(define z1 (cons x x))
(say z1)
;; z1 is a pair whose car and cdr both point to the same pair. In contrast the
;; structure z2 is created by:
(define z2 (cons (list 'a 'b) (list 'a 'b)))
;; In this structure the pairs in the two lists are distinct, although z1 and z2
;; are "the same" list ((a b) a b). 
(say "z1" z1)
(say "z2" z2)
;; A difference that sharing pairs makes can be presented with the following 
;; procedure, which modifies the car of a structure:
(define (set-to-doge! x)
  (set-car! (car x) 'doge)
  x)
;; Even though z1 and z2 are "the same" structure, applying set-to-doge! to them
;; yields different results. With z1 altering the car also alters the cdr, 
;; because they are the same pair. With z2 only the car is modified:
(say "eq? z1" (eq? (car z1) (cdr z1)))
(say "eq? z2" (eq? (car z2) (cdr z2)))
(set-to-doge! z1)
(say "z1" z1)
(set-to-doge! z2)
(say "z2" z2)
;; One way to detect sharing in lists is to use the eq? predicate which tests
;; if two symbols point to the same structure.

;; Exercise 3.17
(define (in-list list elem)
  (cond ((not (pair? (car list))) #f)
        ((eq? (car list) elem) #t)
        (else (in-list (cdr list) elem))))
(say "in-list 1" (in-list z1 (car z1)))
(say "in-list 2" (in-list z1 (list 'a 'b)))

(define (count-pairs x)
  (let ((encountered '()))
    (define (helper x)
      (if (or (not (pair? x)) (memq x encountered))
          0
          (begin
            (set! encountered (cons x encountered))
            (+ (helper (car x))
               (helper (cdr x))
               1))))
    (helper x)))
(say "count-pairs z1" (count-pairs z1))
(say "count-pairs z2" (count-pairs z2))

;; Exercise 3.18
;; TODO

;; Exercise 3.19
;; TODO

;; 3.3.2 Representing queues
;; 
