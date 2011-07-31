;; 1.2 Procedures and processes they generate

;; Square
(define (square n)
  (* n n))

;; 1.2.1 Linear recursion and iteration

;; Recursive factorial function:
;; - n! can be computed by multiplying n and (n-1)!
;; - 1! is equal to 1
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(display (factorial 100))
(newline)

;; n! can be computed by multiplying 1*2*3* ... *(n-1)*n
;; We can first multipy 1*2, then the result by 3,
;; then the result by 4 etc.
(define (factorial2 n)
  (define (factorial-iter product counter max)
    (if (> counter max)
	product
	(factorial-iter (* product counter)
			(+ counter 1)
			max)))
  (factorial-iter 1 1 n))

(display (factorial2 100))
(newline)

;; inc() and dec() - increment and decrement value
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))
;; Add two positive integers
;; Variant 1 - recursive procedure
(define (add1 a b)
  (if (= a 0)
      b
      (inc (add1 (dec a) b))))
(display (add1 13 44))
(newline)

;; Variant 2 - iterative procedure
(define (add2 a b)
  (if (= a 0)
      b
      (add2 (dec a) (inc b))))
(display (add2 33 11))
(newline)

;; Exercise 1.10

;; Ackerman's function is defined as follows:
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(display (A 1 10))
(newline)
(display (A 2 4))
(newline)
(display (A 3 3))
(newline)

;; 1.2.2 Tree recursion

;; An example of tree recursion is computing Fibonacchi numbers, where
;; each number is the sum of two preceding.

;; A recursive procedure for computing the Fibonacchi numbers looks so
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(display (fib 5))
(newline)

;; To compute (fib 5) we compute (fib 4) and (fib 3). To compute (fib 4)
;; we compute (fib 3) and (fib 2) etc.
;; This does so much redundant computation.

;; Iterative version of Fibonacchi bases on applying repeatedly
;; the given simultaneous transformations:
;; a = a + b
;; b = a
(define (fib2 n)
  (define (fib-iter a b count)
    (if (= count 0)
        a
        (fib-iter b (+ a b) (- count 1))))
  (fib-iter 0 1 n))

(display (fib2 100))
(newline)

;; Number of ways in which we can exchange a money amount 'a' given 'n' kinds
;; of coins.
(define (count-change amount)
  ;; Take number of coin types and return the denomination of first kind
  (define (first-denomination coin-types)
    (cond ((= coin-types 1) 1)
          ((= coin-types 2) 5)
          ((= coin-types 3) 10)
          ((= coin-types 4) 25)
          ((= coin-types 5) 50)
          ((= coin-types 6) 100)))
  ;; Recursive approach
  (define (cc amount coin-types)
    ;; If 'a' is 0 than there is only one way to make change
    (cond ((= amount 0) 1)
          ;; There is no way to make change when:
          ;; - value to change is a negative number
          ;; - there are no types of coins to make change from
          ((or (< amount 0) (= coin-types 0)) 0)
          ;; The number of ways to change 'a' amount of money with
          ;; given kinds of coins equals sum of:
          ;; - number of ways to change 'a' using all but the first kind of coin
          (else (+ (cc amount 
                       (- coin-types 1))
                   (cc (- amount
                          (first-denomination coin-types))
                       coin-types)))))
  ;; Start out with 6 coin-types
  (cc amount 6))

(display "Count change: ")
(display (count-change 100))
(newline)

;; Exercise 1.11

;; Function f is defined by the rule:
;; n < 3  : f(n) = n
;; n >= 3 : f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3)

;; Recursive verision
(define (fun1 n)
  (if (< n 3)
      n
      (+ (fun1 (- n 1))
         (* 2 (fun1 (- n 2)))
         (* 3 (fun1 (- n 3))))))

(display (fun1 10))
(newline)

;; Iterative version
;; f(0) = 0
;; f(1) = 1
;; f(2) = 2
;; f(3) = f(2) + 2*f(1) + 3*f(0)
;; f(4) = f(3) + 2*f(2) + 3*f(1)
(define (fun2 n)
  ;; Generic step-value count
  (define (step-value x y z)
    (+ x (* 2 y) (* 3 z)))
  ;; Iteration step
  (define (fun2-iter fn-1 fn-2 fn-3 counter)
    (cond ((< n 3) n)
          ((= counter n) (step-value fn-1 fn-2 fn-3))
          (else (fun2-iter (step-value fn-1 fn-2 fn-3)
                           fn-1
                           fn-2
                           (+ counter 1)))))
  ;; Start loop
  (fun2-iter 2 1 0 3))

(display (fun2 100))
(newline)

;; Exercise 1.12

;; Pascals triangle:
;; - the numbers on the edge of the triangle are all 1
;; - each number inside the triangle is the sum of two numbers above it

;; (define (pascal-triangle n)
;;   ;; generic cell value count
;;   (define (cell-value row i)
;;     (cons (+ (list-ref row (- i 1)) (list-ref row i)) '()))
;; 
;;   ;; generate single row
;;   (define (pascal-triangle-elem row prev_row row_i row_j)
;;     (if (= row_j row_i)
;;         ;; add tail elem and pack result as list
;;         (list (append row 
;;                       (cons 1 '())))
;;         ;; append current value
;;         (pascal-triangle-elem (append row (cell-value prev_row row_j))
;;                               prev_row
;;                               row_i
;;                               (+ row_j 1))))
;;   ;; Generate all rows
;;   (define (pascal-triangle-iter rows row_i)
;;     (if (= row_i n)
;;         ;; All work done :)
;;         rows
;;         ;; Append current row
;;         (pascal-triangle-iter (append rows 
;;                                       ;; Build current row
;;                                       (pascal-triangle-elem (list 1) 
;;                                                             ;; Previous row
;;                                                             (list-ref rows (- (length rows)
;;                                                                               1))
;;                                                             row_i 
;;                                                             1))
;;                               (+ row_i 1))))
;; 
;;   ;; Start loop
;;   (pascal-triangle-iter (list (list 1))
;;                         1))

(define (pascal-triangle n)
  ;; Generic cell value count
  (define (cell-value row i)
    (cons (+ (list-ref row (- i 1)) (list-ref row i)) '()))

  ;; Generate single row
  (define (pascal-triangle-elem row prev_row row_i row_j)
    (if (= row_j row_i)
        ;; Add tail elem
        (append row (cons 1 '()))
        ;; Append current value
        (pascal-triangle-elem (append row 
                                      (cell-value prev_row row_j))
                              prev_row
                              row_i
                              (+ row_j 1))))
  ;; Generate all rows
  (define (pascal-triangle-iter prev_row row_i)
    (if (= row_i n)
        ;; All work done :)
        prev_row
        ;; Append current row
        (pascal-triangle-iter (pascal-triangle-elem (list 1) 
                                                    prev_row row_i 
                                                    1)
                              (+ row_i 1))))

  ;; Start loop
  (pascal-triangle-iter (list 1) 1))

(pascal-triangle 10)

;; 1.2.3 Orders of growth

;; TODO

;; 1.2.4 Exponentation

;; Linear iteration process is defined as follows:
(define (expt b n)
  (define (expt-iter b product counter)
    (if (= counter n)
        product
        (expt-iter b
                   (* b product)
                   (+ counter 1))))
  ;; Start loop
  (expt-iter b 1 0))

(display (expt 2 5))
(newline)

;; We can compute exponentials in fever steps by using successive squaring.
;; b^8 can be computed by using three multiplications:
;; b^2 = b*b
;; b^4 = b^2 * b^2
;; b^8 = b^4 * b^4

;; General rule (also for powers of odd numbers) is:
;; b^n = (b^(n/2))^2 - even
;; b^n = (b*b^(n-1)) - odd
(define (fast-expt b n)
  ;; Is number even?
  (define (even? n)
    (= (remainder n 2) 0))

  ;; Main loop
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(display (fast-expt 10 100))
(newline)

;; Exercise 1.16

;; Design an iterative exponentiation process, that uses successive squaring
;; and uses a logarythmic number of steps.

;; 1.2.5 Greatest common divisors

;; GCD of two integers 'a' and 'b' is the largest integer, that divides both
;; a and b with no remainder. e.g. GCD of 16 and 28 is 4.
;; A common way to solve this problem is by using Euclids Alogorithm.
;; The idea of algorithm is based on the observation, that if 'r' is the
;; remainder when 'a' is divided by 'b', then the common divisors of 'a' and 'b'
;; are precisely the same as comon divisors of 'b' and 'r'.
;; GCG(a,b) = GCD(b,r) where r = a % b.

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(display (gcd 12 14))
(newline)

;; This generates a iterative process, whose number of steps grows as the
;; logarithm of numbers involved. 

;; 1.2.6 Testing for primality

;; By searching for divisors
;; One way to find if a number is prime, is to find its divisors.
;; If 'n' is not prime, then it must have a divisor less than, or equal to sqrt(n).
;; This means, that algorithm needs only to test divisors between 1 and sqrt(n).

(define (smallest-divisor n)
  ;; Check if n divides m
  (define (divides? n m)
    (= (remainder m n) 0))

  ;; Find divisor for given 'n' value
  (define (find-divisor n test)
    (cond ((> (square test) n) n)
          ((divides? test n) test)
          (else (find-divisor n (+ test 1)))))

  (find-divisor n 2))


(define (prime? n)
    ;; divisor of 'n' is equal 'n'
  ;; this means that 'n' is a prime value
  (= n (smallest-divisor n)))

(display "Is 13 prime? ")
(display (prime? 13))
(newline)
(display "Is 34 prime? ")
(display (prime? 34))
(newline)

;; Ferman test algorithm:
;; - choose a random number 'a' between 1 and 'n' - 1
;; - check whether the remainder of a^n modulo n is equal to 'a' 
(define (ferman-test n)
  ;; Compute exponential of a number modulo another number
  (define (expmod base exp m)
    (remainder (fast-expt base exp) m))

  ;; Check the remainder
  (define (try-it rand)
    (= (expmod rand n n) rand))

  ;; Choose a random number
  (try-it (random n)))

;; Run the test 't' times
(define (fast-prime? n t)
  (cond ((= t 0) #t)
        ((ferman-test n) (fast-prime? n (- t 1)))
        (else #f)))

;; Check for UR primes
(display "Fast prime of 123: ")
(display (fast-prime? 123 3))
(newline)

;; Exercise 1.21
;; Apply smallest-divisor to 199 1999 19999
(display "Smallest divisor of 199: ")
(display (smallest-divisor 199))
(newline)
(display "Smallest divisor of 1999: ")
(display (smallest-divisor 1999))
(newline)
(display "Smallest divisor of 19999: ")
(display (smallest-divisor 19999))
(newline)

;; Exercise 1.22
(define (timed-prime-test n)
  ;; Test for primes with time-debug info
  (define (start-prime-test n start-time)
    ;; Report elapsed time
    (define (report-time current-time)
      (display " *** ")
      (display (- current-time start-time)))
    ;; Prime time ;)
    (if (prime? n)
        (report-time (runtime))))

  ;; Start teh test
  (newline)
  (display "Timing prime? for ")
  (display n)
  (start-prime-test n (runtime)))

;; (runtime) not implemented?
;; (timed-prime-test 12345)

