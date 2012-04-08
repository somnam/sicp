;; Helpers
(define (say input) (display input) (newline))

;; 3.0 Modularity, objects and state
;; We need strategies to help us structure large systems, so that they will be
;; modular. So that they can be divided naturally into coherent parts that can
;; be separately developed and maintained.
;; We do this by basing the structure of our programs on the structure of the
;; system being modeled. For each object in the system we construct a
;; corresponding computational object. For each system action we define a
;; symbolic operation in our computational model. In this chapter we will
;; investigate two prominent organizational strategies arising from two rather
;; different world views of the structure of systems:
;; - concentrating on objects (system is a collection of objects whose behaviour
;;   changes over time)
;; - concentrating on streams of information that flow in the system, much as a
;;   signal-processing system


;; 3.1 Assignment and local state
;; We can characterize an objects state by one or more state variables, which
;; maintain enough information about history to determine the objects current
;; behaviour. In a simple banking system we could characterize the state of an
;; account by a current balance rather than by remembering the entire history
;; of account transactions.
;; In a system composed of many objects, the objects are rarely completly
;; independent. Each can influence the states of others through interactions,
;; which serve to couple the state variables of one object to those of other
;; objects.
;; For a system model to be modular it should be decomposed into computational
;; objects that model the actual objects in the system. Each computational object
;; must have its own local state variables, describing the objects actual state.
;; Since the state of objects in the system changes over time, the state variables
;; of corresponding computational objects must also change. The language must
;; provide an assignment operator to enable us to change the value associated
;; with the name.

;; 3.1.1 Local state variables
;; To ilustrate the idea of a computational object with time-varying state, let
;; us model the situaton of withdrawing money from a bank account. We will do
;; this using the procedure withdraw, which takes as an argument the amount to
;; be withdrawn. If there is enough money in the account to accomodate the
;; withdrawal, then withdraw should return the balance remaining after the
;; withdrawal. Otherwise it should return the message 'Insufficient funds'.
;; To implement withdrawal we can use a variable 'balance' to indicate the
;; balance of money in the account and define withdraw as a procedure that
;; accesses 'balance'. It checks if balance is at least as large as the requested
;; amount. If so, it decrements balance by amount and returns the new value of
;; balance. 
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      ;; set! changes 'balance' so that new value is the result of
      ;; evaluating given expression
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
;; The procedure uses the 'begin' special form to cause two expressions to be
;; evaluated in the case where test is true. The value of the final expression
;; is returned as the value of the entire begin form.

;; The balance variable presents a problem. It is defined in the global environment
;; and is freely accessible to be examined or modified by any procedure. It would
;; be much better if we could make balance internal to withdraw, so that any other
;; procedure could accesses balance only through calls to withdraw (indirectly).
(define (make-withdrawal balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds!")))
;; make-withdrawal can be used as follows to create two objects:
(define w1 (make-withdrawal 100))
(define w2 (make-withdrawal 150))
;; Observe that w1 and w2 are completely independent objects, each with its own
;; local state variable balance. Withdrawals from one do not affect the other:
 
;; Representing simple bank accounts boils down to making deposits as well as
;; withdrawals. Here is a procedure that returns a 'bank account object':
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds!"))

  (define (deposit amount)
    (if (> amount 0)
        (begin (set! balance (+ balance amount))
               balance)
        "Incorrect amount!"))

  (lambda (m . a)
    (cond ((eq? m 'w) (withdraw (car a)))
          ((eq? m 'd) (deposit (car a)))
          (else (say "Unknown request!")))))

;; Each call to make-account creates an object with a local state variable
;; 'balance'. The internal procedures access this variable and manipulate it.
;; The object takes a 'message' as the input and returns one of the two local
;; procedures.
;;(define acc1 (make-account 100))
;;(say (acc1 'w 50))
;;(say (acc1 'd 150))
;;(say (acc1 'w 1000))
;; Another call to make-account will produce a completly separate account object
;; which maintains its own local balance.

;; Exercise 3.1
;; Write an accumulator - a procedure that is called repeatedly with a single
;; numeric argument and accumulates its arguments into a sum.
(define (make-accumulator sum)
  (lambda (amount)
    (if (symbol? amount)
        "Incorrect input value!"
        (begin (set! sum (+ sum amount))
               sum))))

;;(define A1 (make-accumulator 5))
;;(say (A1 10))
;;(say (A1 10))
;;(say (A1 -100))
;;(say (A1 'qwwe))


;; Exercise 3.2
;; Count the number of times a given procedure is called. Write a procedure
;; 'make-monitored' that takes as input a procedure 'f', that itself takes input.
;; The result returned by make-monitored is an object, that takes track of the
;; number of times it has been called by maintaining an internal counter.
;; If the input is:
;; - 'how-many-calls? : return the value of the counter
;; - 'reset-count     : resets the counter to 0
;; - other input      ; result of calling 'f' with that input and increments
;;                      the counter
(define (make-monitored f)
  (let ((how-many-calls 0))
    (lambda (input)
      (cond ((and (symbol? input)
                  (eq? input 'how-many-calls?))
             how-many-calls)
            ((and (symbol? input)
                  (eq? input 'reset-count))
             (set! how-many-calls 0))
            (else (begin (set! how-many-calls (+ how-many-calls 1))
                         (f input)))))))

;;(define monitor-sqrt (make-monitored sqrt))
;;(say (monitor-sqrt 'how-many-calls?))
;;(say (monitor-sqrt 10))
;;(say (monitor-sqrt 5))
;;(say (monitor-sqrt 'how-many-calls?))
;;(say (monitor-sqrt 'reset-count))
;;(say (monitor-sqrt 'how-many-calls?))

;; Exercise 3.3 abd 3.4 HAHAHAHA :-)


;; 3.1.2 Benefits of introducing assignment
;; Viewing systems as collections of objects with local state is a powerfull
;; technique for maintaining modular desing. As a simple example consider the
;; desing of a procedure 'rand' that choses an integer at random.

;; It's not at all clear what it is meant "chosen at random". Successive calls
;; to rand should produce a sequence of numbers that has statistical properties
;; of uniform distribution. We can assume that we have a procedure rand-update
;; that has the property that if we start with a given number x1 and form:
;; x2 = (rand-update x1)
;; x3 = (rand-update x2)
;; then the sequence of values x1, x2, x3 ... will have the desired statistical
;; properties. 
;; We can implement rand as a procedure with a local state variable x, that is
;; initialized to some fixed value rand-init. Each call to rand:
;; - computes rand-update of the current value of x
;; - stores the value as the new value of x
;; - returns this as the random number
(define rand-init 137)
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))
;; Compute rand value as stated above
(define rand
  (let ((x rand-init))
    (lambda ( . args)
      (if (pair? args)
          (set! x (car args))
          (begin
            (set! x (rand-update x))
            x)))))

;; To realize the annoyance that would be to explicitly remember the current
;; value of 'x' passed as an argument to 'random-update', we will consider
;; using random numbers to implement a technique called Monte Carlo simulation
;; (YAY! :/).

;; Quote:
;; "The Monte Carlo method consists of choosing sample experiments at random from
;; a large set and then making deductions on the basis of the probabilities 
;; estimated from tabulating the results of those experiments."
;; I don't get it ...
;; For example we can approximate Pi by using the fact, that 6/Pi^2 is the
;; probability that two integers chosen at random will have no factors in
;; common - their greatest common divisor will be 1.
;; To obtain the approximation to Pi, we perform a large number of experiments.
;; In each experiment we choose two integers at random and perform a test to
;; see if their GCD is 1. The fraction of times, that the test passes, gives us
;; our estimate of 6/Pi^2, and from this we obtain our approximation to Pi.

;; The heart of our program is the procedure monte-carlo, which takes as 
;; arguments the number of times to try and experiment together with the
;; experiment procedure. The procedure doesn't have any arguments and returns
;; true or false each time it is run. The monte-carlo procedure returns
;; a number telling the fraction of the trials in which the experiment was 
;; found true.
(define (monte-carlo trials experiment)
  (define (iter-trials trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter-trials (- trials-remaining 1)
                        (+ trials-passed 1)))
          (else
            (iter-trials (- trials-remaining)
                         trials-passed))))
  (iter-trials trials 0))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

;; Will this work?
;;(say (estimate-pi 10))

;; The general phenomenon illustrated by the Monte Carlo example is this:
;; From the point of view of one part of a complex process, the other parts
;; appear to change within time. They have hidden time-varying local state.
;; We make computational objects (such as random-number generators) whose 
;; behaviour changes within time. We model state with local state variables, 
;; and we model the changes of state with assignments to those variables.

;; Exercise 3.5 TODO

;; Exercise 3.6
(define (rand2 . args)
  (if (pair? args)
      ;; Argument given
      (let ((operation (car args)))
        (cond ;; Generate new random numer
              ((and (symbol? operation)
                    (eq? operation 'generate))
               (rand))
              ;; Rests the internal state variable
              ((and (symbol? operation)
                    (eq? operation 'reset))
               (lambda (new-rand-init)
                 (rand new-rand-init)))))
      ;; No arguments - by default return next rand value
      (rand)))

;;(say "(rand) test cases:")
;;(say (rand2)) ;; 42
;;(say (rand2)) ;; 17
;;(say ((rand2 'reset) rand-init))
;;(say (rand2)) ;; 42
;;(say (rand2)) ;; 17

;; 3.1.3 The cost of introducing assignment
;; The set! operation enables us to model objects that have local state.
;; But so long as we don't use assignments, two evaluations of the same
;; procedure with the same arguments will produce the same result. So that
;; procedures can be viewed as computing mathematical functions.
;; Programming without any use of assignments is accordingly known as (tadam...)
;; functional programming.
;; Substitution is based on the notion, that the symbols in our language are
;; essentially names for values. But as soon as we introduce set! and the
;; idea that the value of a variable can change, a variable can no longer be
;; simply a name. Now a variable refers to a place, where a value can be stored
;; and the value at this place can change.
;; As soon as we introduce change into our computational models, many notions
;; that were previously straightforward become problematical. One of them is
;; the concept of being "the same".
(define (make-decrementer balance)
  (lambda (amount) (- balance amount)))
;; We can call (make-decrementer) twice with the same argument to create two
;; procedures:
(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))
;; Are D1 and D2 the same ? An acceptable answer is yes, because D1 and D2 have
;; the same computational behavior - each substracts 25 from its input. 
;; D1 could be substituted for D2 in any computation without changing the
;; result.
;; Contrast this with making two calls to (make-simplified-withdraw).
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))
;; W1 and W2 are not the same, because subsequent calls to them have different
;; effects:
;;(say (W1 20)) ;; 5
;;(say (W1 20)) ;; -15
;;(say (W2 20)) ;; 5, should be -35
;; Even though W1 and W2 were created by evaluating the same expression:
;; (make-simplified-withdraw 25)
;; it is not true, that W1 could be substituted for W2 in any expression
;; without changing the result of evaluating the expression.
;; A language is said to be referentially transparent when is supports the
;; concept, that a procedure call can be replaced by the call value in an 
;; expression without changing the value of the expression. Referential
;; transparency is violated when we include set! in our programming language.
;; This makes it tricky to determine when we can simplify expressions by
;; substituting equivalent expressions. 

;; Pitfalls of imperative programming
;; In contrast to functional programming, programming that makes extensive use
;; of assignment is known as imperative programming. Programs written in this
;; style are open to bugs that cannot occur in functional programs. Programming
;; with assignments forces us to carefully consider the relative orders of the
;; assignments to make sure that each statement is using the correct version of
;; the variables that have been changed. For example, recall the iterative
;; factorial program:
(define (factorial n)
  (define (factorial-iter step result)
    (if (> step n)
        result
        (factorial-iter (+ step 1)
                        (* result step))))

  (factorial-iter 1 1))
;;(say (factorial 5))

;; Instead of passing arguments in the internal loop we could adopt a more
;; imperative style by using assignmen to update the values of variables:
(define (factorial n)
  (let ((result 1)
        (step 1))
    (define (iter)
      (if (> step n)
          result
          (begin (set! result (* result step))
                 (set! step   (+ step 1))
                 (iter))))
    (iter)))
;;(say (factorial 5))

;; This does not change the results produced by the program, but does introduce
;; a subtle trap. If the order of assignments was different, then the program
;; would generate an incorrect result. This issue does not arise in functional
;; programs. The complexity of imperative programs becomes even worse if we 
;; consider applications in which several processes execute concurrently.

;; Exercise 3.7 
;; TODO

;; Exercise 3.8 
;; (f 0) + (f 1) = 0;
;; (f 1) + (f 0) = 1;
;; (1 + value) * factor
(define (make-f)
  (let ((factor '()))
    (lambda (value)
        (and (null? factor) (set! factor value))
        (* factor value))))

;;(define f (make-f))
;;(say (+ (f 0) (f 1)))
;;(define f (make-f))
;;(say (+ (f 1) (f 0)))

