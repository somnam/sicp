;; 2.1.1 Arithmetic operations for rational numbers

;; Data abstraction - a methodology, that enables us to isolate how a compound
;; data object is used from the details of how it is constructed from primitive
;; data objects. Programs should use compound data objects is such a way
;; as to make no assumptions about this data. At the same time a 'concrete'
;; data representation is defined indepenedent of the programs, that use
;; the data. The interface between those two parts of our system will be
;; a set of procedures, called 'selectors' and 'constructors', that implement
;; the abstract data in terms of concrete representation.

;; To ilustrate this technique we will consider how to desing a set of
;; procedures for manipulating rational numbers. We want to be able to:
;; - add
;; - substract
;; - multiply
;; - divide
;; - test for equality

;; Scheme provides a compound data structure, called a pair. It can be
;; constructed with the primitive procedure 'cons'. This procedure takes
;; two arguments and returns a compound data object, that contains the two
;; arguments as pairs.
(define x (cons 1 2))

;; We can extract the parts using primitive procedures 'car', 'cdr'
(display "Pair datatype: ")
(display x)
(newline)

(display "First pair element: ")
(display (car x))
(newline)

(display "Second pair element: ")
(display (cdr x))

;; Data objects constructed from pairs are called *list-structured* data.
;; This is the only glue we need to construct rational numbers.
;; We simply represent a rational number as a pair of two integers:
;; - a numerator
;; - a denominator

;; GCD used in make-rat - to reduce rational numbers to lowest terms.
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Create a rational number object
(define (make-rat n d)
  ;; Count gcd for numerator and denominator
  (let ((g (abs (gcd n d))))
    ;; Negative rational numbers have only a negative numerator
    (if (or (negative? n) (negative? d))
        (cons (/ (- (abs n)) g)
              (/ (abs d) g))
        (cons (/ n g)
              (/ d g)))))

;; Get numerator part
(define (numer x)
  (car x))

;; Get denominator part
(define (denom x)
  (cdr x))

;; Print rational number
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Add two rational numbers
;; n1/d1 + n2/d2 = (n1*d2 + n2*d1)/d1*d2
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x)
               (denom y))))

;; Substract two rational numbers
;; n1/d1 - n2/d2 = (n1*d2 - n2*d1)/d1*d2
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x)
               (denom y))))

;; Multipy two rational numbers
;; n1/d1 * n2/d2 = n1*n2/d1*d2
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

;; Divide two rational numbers
;; n1/d1/n2/d2 = n1*d2/d1*n2
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

;; Compare two rational numbers
;; == only if n1*d2 = n2*d1
(define (eql-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; Try it out
(define one-half
  (make-rat 1 2))
(define one-third
  (make-rat 1 3))

(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (sub-rat one-half one-third))
(print-rat (sub-rat one-third one-half))
(print-rat (mul-rat one-half one-third))
(print-rat (div-rat one-half one-third))
(newline)
(display (eql-rat? one-half one-half))
(newline)
(display (eql-rat? one-half one-third))

;; Exercise 2.2
;; TODO

;; Exercise 2.3
;; TODO

;; 2.1.3 What is meant by data?

;; In general, we can think of data as defined by a collection of selectors and
;; constructors, together with specified conditions, that these procedures must
;; fulfill in order to be a valid representation.
;; The earlier created set of procedures - make-rat, number and denom must satisfy
;; a condition that, for any integer 'n' an any non-zero integer 'd',
;; if 'x' is (make-rat n d) then
;; (number x)/(denom x) = n/d

;; This point of view can serve not only to define 'high-level' data objects, but
;; also low-level ones, like the 'pair' compound datatype. The language supplies
;; procedures 'cons', 'car' and 'cdr' for operating on pairs. These operations satisfy
;; the following condition:
;; for any objects x and y, if z is (cons x y) then (car z) is x and (cdr z) is y.
;; Any triple of procedures, that satisfy the above condition can be used as the
;; basis for implementing pairs.
;; We can implement cons, car and cdr without using any datatype, but only procedures.

;; Datatype constructor
(define (cons2 x y)
  ;; Closure thingy with use of lambda procedure :)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (display "Error !!")
                (newline)))))

;; Datatype accessors
(define (car2 x)
  (x 0))
(define (cdr2 x)
  (x 1))

;; The value returned by cons2 is a procedure - defined internally.
(define pair1 (cons2 1 2))
(newline)
(display "First element from pair: ")
(display (car2 pair1))
(newline)
(display "Second element from pair: ")
(display (cdr2 pair1))
(newline)


;; This procedural representation of datatype is valid and satisfies
;; all required conditions for implementation of a pair dataset.
;; We cannot distinguish it from one that uses 'real' data structures.
;; This example displays, that the ability to manipulate procedures as
;; objects automatically provides ability to represent compound data.
;; This style of programming is called 'message passing'.

;; Exercise 2.4
(define (cons3 x y)
  (lambda (m) (m x y)))
(define (car3 z)
  (z (lambda (p q) p)))
(define (cdr3 z)
  (z (lambda (p q) q)))

;; Exercise 2.5
;; TODO

;; Exercise 2.6
;; TODO

;; 2.1.4 Interval arithmetic
;; The tales of Alyssa P. Hacker.
;; Alyssa is designing a system, which allows to manipulate inexact quantities.
;; whith known presision - such as measures parameters of physical devices. The
;; results must be numbers of known precission.
;; The idea to implement 'interval arithmetic' is based on a set of arithmetic
;; operations for combining ranges of possible values for an inexact quantity.
;; The result of arithmetic operations on two intervals is itself an interval,
;; representing the range, of the result.
;; An interval (or range) can be treated as an objet, that has two endpoints:
;; - lower bound
;; - upper bound

;; Constructor
(define (make-interval a b)
  (cons a b))
;; Exercise 2.7 - Selectors
(define (upper-bound x)
  (car x))
(define (lower-bound x)
  (cdr x))

;; Addition
;; minimum value = sum of lower bounds
;; maximum value = sum of upper bounds
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; Multiplication
;; minimum value = min of product of bounds
;; maximum value = max of product of bounds
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; Division
;; Multiply the first interval by the reciprocal of the second.
;; Reciprocal of the second interval is:
;;   minimum value = reciprocal of the upper bound
;;   maximum value = reciprocal of the lower bound
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (lower-bound y))
                               (/ 1.0 (upper-bound y)))))

;; Exercise 2.8 - 2.16
;; TODO

