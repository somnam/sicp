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
;; The primitive mutators for pairs are set-car! and set-cdr!. The first one 
;; takes two arguments, first of which must be a pair. It replaces the car
;; pointer of this pair by the second argument of set-car!.
;; x and y are bound to given lists:
(define x (list (list 'a 'b) 'c 'd 'g 'h 'i))
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
;; a procedure get-new-pair which returns a new pair that is not part of any
;; existing list structure.
(define (get-new-pair) (cons '() '()))
(define (cons2 x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))
(say "cons" (cons2 1 2))

;; Exercise 3.12
;; The procedure for appending two lists is as follows:
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))
;; The procedure append! is similar to append, but it's a mutator, that appends
;; two lists by splicing them together. It modifies the final pair of x so that
;; its cdr is now y.
(define (last-pair x)
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
;; One way to detect sharing in lists is to use the eq? predicate which tests
;; if two symbols point to the same structure.
(say "eq? z1" (eq? (car z1) (cdr z1)))
(say "eq? z2" (eq? (car z2) (cdr z2)))
(set-to-doge! z1)
(say "z1" z1)
(set-to-doge! z2)
(say "z2" z2)

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
      ;; Current item is the cdr of the last element in list or was already
      ;; marked as encountered when traversing the pairs list.
      (if (or (not (pair? x)) (memq x encountered))
          0
          ;; The begin block is a wrapper for evaluating multiple expressions
          ;; in a "true" or "false" if block.
          (begin
            ;; Append newli encountered item to list of encountered elements.
            (set! encountered (cons x encountered))
            (+ (helper (car x))
               (helper (cdr x))
               ;; Start with 1 for the input list pair of car and cdr.
               1))))
    (helper x)))
(say "count-pairs z1" (count-pairs z1))
(say "count-pairs z2" (count-pairs z2))

;; Exercise 3.18
;; TODO

;; Exercise 3.19
;; TODO

;; 3.3.2 Representing queues
;; The mutators set-car! and set-cdr! enable us to use pairs when constructing
;; data sctructures impossible to build with cons, car and cdr alone.
;; One of them is a queue. It's a sequence in which items are inserted at one
;; end (rear) and deleted from the other end (front) - it's a FIFO type queue.
;; We can define it using the following operations:
;; * a constructor (make-queue) returns an empty queue
;; * a selector testing if the queue is empty (empty-queue? <queue>)
;; * a selector returning the object at the front of the queue, throwing an
;;   error if it's empty
;; * a mutator (insert-queue! <queue> <item>) inserting the item at the end
;;   of the queue and returing it
;; * a mutator (delete-queue! <queue>) removing the item at the front of the
;;   queue and returing it, throws an error if the queue is empty beforehand

;; The implementation will use a list representation with a simple modification
;; which allows the queue operations to be implemeted so that they require O(1)
;; steps. The difficulty in the list representation arises from the need to scan
;; to find the end of the list. We only have a pointer to the beginning of the
;; list. The modification that avoids this drawback is an additional pointer to
;; the final part of the list. When inserting an item, we can use this pointer
;; to avoid scanning the whole list.
;; A queue is represented as a pair of pointers - front-ptr and rear-ptr.
;; We can use cons to combine the two pointers, thus creating a queue.
;; Queue procedures:
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
;; Queue definition:
(define (make-queue) (cons '() '()))
;; To select the item at the front of the queue we return the car of the pair
;; indicated by the front pointer:
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Function called with an empty queue" queue)
      (car (front-ptr queue))))
;; Insert an item in a queue:
;; - create a new pair whose car is the item to be inserted, and cdr is empty
;; - if the queue is empty we set the front and rear pointers to this item
;; - if not, we modfy the final pair in the queue to point to the new pair
;;   and set the rear-ptr to it
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))
;; To delete an item at the front of the queue we modify the front-ptr so that
;; it points to the second item in the queue, accessible by the first items cdr.
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "delete-queue! called with an empty queue." queue))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))
;; Exercise 3.21
(define (print-queue queue)
  (define (printer queue-item)
    (cond ((null? queue-item)
           (newline))
          (else
            (display (car queue-item))
            (printer (cdr queue-item)))))
  (if (empty-queue? queue)
      (begin (display '()) (newline))
      (printer (front-ptr queue))))

;; Test
(define q1 (make-queue))
(say "queue" q1)
(insert-queue! q1 'a)
(display "insert-queue! ")
(print-queue q1)
(insert-queue! q1 'b)
(display "insert-queue! ")
(print-queue q1)
(insert-queue! q1 'c)
(display "insert-queue! ")
(print-queue q1)
(delete-queue! q1)
(display "delete-queue! ")
(print-queue q1)
(delete-queue! q1)
(display "delete-queue! ")
(print-queue q1)
(delete-queue! q1)
(display "delete-queue! ")
(print-queue q1)
