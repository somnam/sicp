;; 1.3 Formulating abstractions with higher-order procedures

;; Procedures, that accept other procedures as arguments or return
;; procedures as values are called higher-order-procedures

;; Compute sum of integers form 'a' to 'b'
(define (sum-integers a b)
  ;; Make iteration not war
  (define (sum-iter result step)
    (if (> step b)
        result
        (sum-iter (+ result step)
                  (+ step 1))))

  ;; Start loop
  (sum-iter 0 a))

(display "Sum integers: ")
(display (sum-integers 3 10))
(newline)

;; Compute sum of cubes of integers from 'a' to 'b'
(define (sum-cubes a b)
  ;; Yay
  (define (cube x)
    (* x x x))

  ;; Make more iteration
  (define (sum-cubes-iter result step)
    (if (> step b)
        result
        (sum-cubes-iter (+ result (cube step))
                        (+ step 1))))

  ;; Start loop
  (sum-cubes-iter 0 a))

(display "Sub cubes of integers: ")
(display (sum-cubes 3 10))
(newline)

;; Compute sum of a sequence of terms in series
;; 1/1*3 + 1/5*7 + 1/9*11 + ...
(define (sum-pi a b)
  ;; Generic next value
  (define (seq-val x)
    (/ 1.0 (* x (+ x 2))))

  ;; Loop body
  (define (sum-pi-iter result step)
    (if (> step b)
        result
        (sum-pi-iter (+ result (seq-val step))
                           (+ step 4))))

  ;; Start loop
  (sum-pi-iter 0 a))

(display "Sub pi of integers: ")
(display (sum-pi 3 10))
(newline)

;; Template for summing f(a) + ... + f(b)
(define (gen-sum a b fun inc)
  ;; Make generic iteration not war
  (define (gen-iter result step)
    (if (> step b)
        result
        (gen-iter (+ result (fun step))
                  (inc step))))

  ;; Start loop
  (gen-iter 0 a))

;; Rewite latter methods
(define (sum-integers2 a b)
  (define (identity x)
    x)
  (define (increment x)
    (+ x 1))
  (gen-sum a b identity increment))

(display "Sum integers2: ")
(display (sum-integers2 3 10))
(newline)

(define (sum-cubes2 a b)
  (define (cube x)
    (* x x x))
  (define (increment x)
    (+ x 1))
  (gen-sum a b cube increment))

(display "Sub cubes of integers2: ")
(display (sum-cubes2 3 10))
(newline)

(define (sum-pi2 a b)
  (define (seq-val x)
    (/ 1.0 (* x (+ x 2))))
  (define (val-next x)
    (+ x 4))

  (gen-sum a b seq-val val-next))

(display "Sub pi of integers2: ")
(display (sum-pi2 3 10))
(newline)

;; Pi approx
(display "Pi approximation: ")
(display (* 8 (sum-pi2 1 1000)))
(newline)

;; Approximate integral of a function in range [a,b]
(define (integral a b f dx)
  (define (add-dx x)
    (+ x dx))

  (* (gen-sum (+ a (/ dx 2.0)) b f add-dx)
     dx))

(define (cube x) (* x x x))
(display "Integral of 'cube' in [0,1]: ")
(display (integral 0 1 cube 0.01))
(newline)

;; Exercise 1.31

;; Generic template for product of function values
;; f(a) * ... * f(b)
(define (gen-product a b fun inc)
  ;; Make iteration not war
  (define (gen-iter result step)
    (cond ((= result 0) result)
          ((> step b) result)
          (else (gen-iter (* result (fun step))
                          (inc step)))))

  ;; Start loop
  (gen-iter 1 a))

;; Product of integers from a to b
(define (prod-integers a b)
  (define (identity x)
    x)
  (define (increment x)
    (+ x 1))
  (gen-product a b identity increment))

(display "Product integers: ")
(display (prod-integers 1 10))
(newline)

;; Factorial of integer a
(define (factorial a)
  (define (identity x)
    x)
  (define (increment x)
    (+ x 1))
  (gen-product 1 a identity increment))

(display "Factorial integer: ")
(display (factorial 44))
(newline)

;; Another pi approx
(define (pi-approx a)
  (define (val x)
    (/ (* x x) (* (+ x 1) (+ x 1))))
  (define (next x)
    (+ x 2))
  (* 2 (gen-product 2.0 a val next)))

;; It doesn't work :(
(display "Another pi approx: ")
(display (pi-approx 100))
(newline)

;; Exercise 1.32
;; TODO

;; Exercise 1.33
;; TODO

;; 1.3.2 Constructing procedures using 'Lambda'

;; 'lambda' is used to express procedures in place without the need of
;; defining an auxilary procedure.
;; The 'sum-pi' procedure can be rewritten as so:
(define (sum-pi2 a b)
  (gen-sum a
           b
           (lambda (x) (/ 1.0 (* x (+ x 2))))
           (lambda (x) (+ x 4))))

(display "Sum Pi of integers lambda: ")
(display (sum-pi2 3 10))
(newline)

;; In general lambda is used to create procedures in the same way as 'define'
;; except that no name is specfied for the procedure:
;; (lambda (<formal-parameters>) body)
;; The only difference is that the 'lambda' procedure has not been associated
;; with any name in the environment.
;; in fact
(define (plus4 x)
  (+ x 4))
;; is equivalent to
(define plus4
  (lambda (x) (+ x 4)))

;; Local variables

;; Let's say we compute the function:
;; f(x,y) = x * (1 + x*y)^2 + y*(1 - y) + (1 + x*y)(1 - y)
;; we can express it as
;; f(x,y) = x*a^2 + y*b + a*b
;; where a = (1 + x*y) and b = (1 - y)
;; One way to acomplish this is to define an auxilary procedure to bind the local variables
(define (f x y)
  (define (f-aux a b)
    (+ (* x (* a a))
       (* y b)
       (* a b)))
  (f-aux (+ 1 (* x y))
         (- 1 y)))

;; But using 'let' there is no need to define an auxiliary procedure for binging
;; values to local names.
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (* a a))
       (* y b)
       (* a b))))

;; The first part of the 'let' expression is a list of name-value pairs
;; The body of the 'let' is evaluated with these names bound as local variables.
;; A 'let' expression is really syntatic sugar for the underlying 'lambda'
;; application.

;; Exercise 1.34
(define (f g)
  (g 2))
(display (f (lambda (x) (* x (+ x 1)))))
(newline)

;; 1.3.3 Procedures as general methods

;; This topic will be discussed based on half-interval method.
;; It is a simple technique for finding roots of an equation f(x) = 0,
;; where 'f' is a continuous function. If we are given points a and b
;; such that f(a) < 0 < f(b) then f(x) must have at least a one zero
;; between a and b. To locate the zero:
;; - let x be average of a and b
;; - compute f(x):
;; - if f(x)> 0 then f must have a zero between a and x
;; - if f(x) < 0 then f must have a zero between x and b
(define (search-for-root f neg pos)
  ;; On success msg
  (define (result midpoint)
    (display "Aproximate root is ")
    (display midpoint)
    (display ".")
    (newline))
  ;; Calculate average of two values
  (define (average a b)
    (/ (+ a b) 2.0))
  ;; Define loop break rule
  (define (good-enough? a b)
    (< (abs (- a b)) 0.001))
  ;; Compute the midpoint of given two values
  (let ((midpoint (average neg pos)))
    ;; Check if the point is close enough to root value
    (if (good-enough? neg pos)
        (result midpoint)
        ;; Run next iteration step based on midpoints sign
        (let ((test-val (f midpoint)))
          ;; Midpoint is positive
          (cond ((positive? test-val)
                 (search-for-root f neg midpoint))
                ;; Midpoint is negative
                ((negative? test-val)
                 (search-for-root f midpoint pos))
                ;; The search value may be equal to 0
                ;; this is the value we're searching for
                (else (result midpoint)))))))

;; We assume that given 'a' and 'b' points meet the
;; f(a) < 0 < f(b) rule. For [a,b] pairs that don't meet
;; this requirement we get false roots. A simple workaround for checking
;; values of f(a) and f(b) before sending the range to 'search-for-root'
(define (half-iterval-method f a b)
  ;; Error msg
  (define (error a b)
    (display "Error: given [")
    (display a)
    (display ",")
    (display b)
    (display "] range is not correct!")
    (newline))
  (let ((f-a (f a))
        (f-b (f b)))
    (cond ((and (negative? f-a)
                (positive? f-b))
           (search-for-root f a b))
          ((and (positive? f-a)
                (negative? f-b))
           (search-for-root f b a))
          (else (error f-a f-b)))))

;; Use the half interval method to aproximate Pi as a root between 2 and 4 of sin(x)
(half-iterval-method sin 2 4)
(half-iterval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)

;; Finding fixed points of functions
;; A 'x' is called a fixed point of function if f(x) = x.
;; For some functions we can locate a fixed point by beginning with an initial
;; guess and applying f(x) repeatedly e.g. f(x), f(f(x)), f(f(f(x))) etc.
;; until the result value does not change much. A simple procedure, that takes
;; as input the function and initial guess value looks like so:
(define (fixed-point f first-guess debug)
  ;; Define tolerance level - loop break rule
  (define (good-enough? a b)
    (< (abs (- a b)) 0.00001))
  ;; Debug message
  (define (dump n)
    (cond ((= debug 1)
           (display "Guess: ")
           (display n)
           (newline))))
  ;; Loop body
  (define (try guess)
    ;; next = f(guess)
    (let ((next (f guess)))
      (dump next)
      ;; Check difference between current and next value
      (if (good-enough? guess next)
          ;; Difference lies within tolerance level
          next
          (try next))))
  ;; Start loop with initial guess
  (try first-guess))

;; Sin(x)?
(display "Fixed point of cos(x) is: ")
(display (fixed-point cos 1.0 0))
(newline)
;; y = sin(y) + cos(y) (f(y) = y, fixed point)
(display "Fixed point of facy f(x) is: ")
(display (fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0 0))
(newline)

;; Computing square root of some number 'x' requires finding a 'y'
;; such that y^2 = x:
;; y = x/y (this translation creates an infinite loop...)
;; 2y = x/y + y
;; y = (x/y + y)/2 (but this does not :))
;; f(y) = y and f(y) = (x/y + y)/2
(define (sqrt x)
  ;; Calculate average of two values
  (define (average a b)
    (/ (+ a b) 2.0))
  (fixed-point (lambda (y) (average y (/ x y))) 1.0 0))

(display "Sqrt of 4.0 is: ")
(display (sqrt 4.0))
(newline)

;; This technique of averaging successive approximations to a solution is called
;; average-damping. 

;; Exercise 1.35
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0 0))

(display "Golden ratio is a fixed point of x -> 1 + 1/x: ")
(display (golden-ratio))
(newline)

;; Exercise 1.36
(display "Solution to x^x = 1000: ")
(newline)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0 0)

;; Exercise 1.37
;; TODO

;; Exercise 1.38
;; TODO

;; Exercise 1.39
;; TODO

;; 1.3.4 Procedures as return values

;; Average dumping is a useful general technique in itself. Given a function
;; f(x) we consider the function, whose value at x is equal to average of
;; x and f(x). We can express this idea by means of:
(define (average-damp f)
  ;; Calculate average of two values
  (define (average a b)
    (/ (+ a b) 2.0))
  ;; Calculat average of x and f(x)
  (lambda (x) (average x (f x))))

;; Applying this procedure to 'square' at value 10 returns 55:
(define (square x) (* x x))
(display "Average damp of square: ")
(display ((average-damp square) 10))
(newline)

;; Using 'average-damp' we can reformulate the sqrt procedure as follows:
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0 0))

(display "Average damp used as sqare root: ")
(display (sqrt 61))
(newline)

;; The cube root of x is a fixed point of the function y -> x/y^2
(define (curt x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y)))) 1.0 0))

(display "Average damp used as cube root: ")
(display (curt 61))
(newline)

;; This formation makes use of three ideas:
;; - function y -> x/y^2
;; - fixed point
;; - average damping

;; Newtons method

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OMG, more mathz         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Finding square roots is a special case of use of Newtons Method.
;; Newtons method is the use of the fixed-point method to approximate
;; a solution of the equation by finding a fixed point of function gsse.
;; If x->g(x) is a differentiable function, then a soluton to the
;; equation g(x) = 0 is a fixed point of the funtion f(x) where
;; f(x) = x - g(x)/Dg(x)
;; Dg(x) is the derivative of g evaluated at x.
;; A derivative is something ;) that transforms a function into another
;; function. If g is a function and dx is a small number, then the
;; derivative Dg of g is the function, whose value at any number x is
;; given by
;; Dg(x) = g(x + dx) - g(x) / dx
(define (D g)
  (let ((dx 0.00001))
    (lambda (x) (/ (- (g (+ x dx)) (g x))
                   dx))))

;; Approximate the derivative of x^3
(define (cube x) (* x x x))
(display "Derivative of x^3 @ 5: ")
(display ((D cube) 5))
(newline)

;; With aid of D we can express Newtons method as a fixed point process
(define (newtons-method g guess)
  ;; g(x) -> f(x)
  (define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((D g) x)))))
  ;; Fixed point process
  (fixed-point (newton-transform g) guess 0))

;; Find solution to f(y) = y^2 - x via Newtons method
(define (sqrt-n x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(display "Newtons method, sqare root of 10: ")
(display (sqrt-n 10))
(newline)

;; Some of rights and priviliges of first-class methods are:
;; - they may be named by variables
;; - they may be passed as arguments to procedures
;; - they may be returned as the result of procedures
;; - they may be inculded in data structures

;; Exercise 1.40
;; (newtons-method (cubic a b c) 1)
(define (cubic a b c)
  (define (cube x)
    (* x x x))
  (define (square x)
    (* x x))
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(display "Solve x^3 + 5x^2 + 6x + 7: ")
(display (newtons-method (cubic 5 6 7) 1.0))
(newline)

;; Exercise 1.41
(define (inc x) (+ x 2))
(define (double f)
  (lambda (x) (f (f x))))

(display "Quad damage: ")
(display (((double (double double)) inc) 5)) 
(newline)

;; Exercise 1.42
;; x -> f(g(x))
(define (compose f g)
  (lambda (x) (f (g x))))

(display "Compose f(x) and g(x): ")
(display ((compose square inc) 6))
(newline)

;; Exercise 1.43
;; x -> f(f(..(f(x))..))
(define (repeated f n)
  (define (repeated-iter r count)
    (if (= count n)
        r
        (repeated-iter (compose f r)
                       (+ count 1))))

  (repeated-iter f 1))

(display "Sqare 5 two times: ")
(display ((repeated square 2) 5))
(newline)

;; Exercise 1.44
;; x -> f(x - dx) + f(x) + f(x + dx) / 3
(define (smooth f)
  (let ((dx 0.0001))
    (lambda (x) (/ (+ (f (- x dx))
                      (f x)
                      (f (+ x dx)))
                   3))))

(display "Smooth out square: ")
(display ((smooth square) 5))
(newline)

(display "Smooth out inc: ")
(display ((repeated (smooth inc) 5) 5))
(newline)

;; Exercise 1.45
;; TODO

;; Exercise 1.46
;; TODO

