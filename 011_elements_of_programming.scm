;; 1.1 Elements of programming

;; 1.1.1 Expressions

;; Primitive number expression
(display 486)
(newline)

;; Compound expression: numbers combined with primitive procedures
(display (+ 12 234))
(newline)
(display (- 1000 334))
(newline)
(display (* 5 99))
(newline)
(display (/ 10 4))
(newline)
(display (+ 2.7 10.1))
(newline)

;; These expressions are called combinations. The leftmost element
;; is the operator and the other elements are called operands.
;; Value of the combination is obtained by applying the operator to
;; values of operands (arguments). This is an example of prefix-notation.

;; Combinations can be nested.

(display (+ (+ 3 5)
	    (- 10 6)))

(display (+ (* 3
	       (+ (* 2 4)
		  (+ 3 5)))
	    (+ (- 10 7)
	       6)))

;; 1.1.2 Naming and the Environment

;; A name identifies a variable, whose value is the object. Assinginig values
;; is done via 'definie' expression.
(define pi 3.14159)
(define radius 3.33)
(display (* pi
	    (* radius
	       radius)))
(newline)

1;; Name-object associations can be created incrementally
;; Here the 'circumference' association uses both 'pi' and 'radius' definitions
(define circumference
  (* 2 pi radius))

(display circumference)
(newline)

;; The posibility of associating values with symbols and retrieving them means
;; that the interpreter must maintain a memory, that keeps track of
;; the name-object pairs. It's called the 'global environment'.

;; 1.1.4 Compound procedures

;; Procedure definitions - an abstraction technique, by which a compound
;; operation can be given a name and then referred to as a unit.
;; Example procedure produces the square root of a given value
;; The value to be multiplied is given a local name 'x'.
(define (square x)
  (* x x))
(define (cube x)
  (* x x x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;; DAMN, a lot of stuff got somehow deleted :/.                          ;;
;; I'll continue from where I left, but must rewrite that stuff someday. ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Square root aproximation

;; Calculate average value of two
(define (average x y)
  (/ (+ x y) 2))

;; Is current square root aproximation 'good enough' ?
;; e.g. differs from expected value with a tolerance level of 0.0001
(define (sqrt-good-enough? x guess)
  (< (abs (- (square guess) x)) 0.0001))

;; Improve current guess value of square root by taking the average of
;; the guess and it's quotient with the squared number
(define (sqrt-improve x guess)
  (average guess (/ x guess)))

;; Formalize the process of guessing a square root for given value
(define (sqrt-iter x guess)
  (if (sqrt-good-enough? x guess)
      guess
      (sqrt-iter x (sqrt-improve x guess))))

;; Assume that a root guess for any value is 1.0
(define (sqrt x)
  (sqrt-iter x 1.0))

;; Try out square root guessing on some values
(display (sqrt 9))
(newline)

;; Cube root aproximation

;; Is current cube root aproximation 'good enough' ?
(define (curt-good-enough? x guess)
  (< (abs (- (cube guess) x)) 0.0001))

;; Improve current guess value of cube root by using formula
;; x/y^2 + 2*y / 3
;; x - value, y - aproximation
(define (curt-improve x guess)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

;; Formalize the process
(define (curt-iter x guess)
  (if (curt-good-enough? x guess)
      guess
      (curt-iter x (curt-improve x guess))))

;; Assume that a cube root guess for any value is 1.0
(define (curt x)
  (curt-iter x 1.0))

;; Try it out
(display (curt 10))
(newline)
(display (curt 33))
(newline)
(display (curt 333333))
(newline)

;; 1.1.8 Procedures as black box abstractions

;; A procedure can have internal definitions, that are local only to
;; the procedure. This helps to break up large programs into tractable
;; pieces. The 'sqrt' procedure can be rewritten as so:
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
;; It is not necessary to pass 'x' explicitly to each of the innernal
;; procedures. 'x' is a free variable in the internal definitions, but
;; also a bound variable for the 'sqrt' definition - it has a local scope
;; of this definition.
