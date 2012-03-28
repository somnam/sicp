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
;; To realize the annoyance that would be to explicitly remember the current
;; value of 'x' passed as an argument to 'random-update', we will consider
;; using random numbers to implement a technique called Monte Carlo simulation
;; (YAY! :/).

;; Quote:
;; "The Monte Carlo method consists of choosing sample experiments at random from
;; a large set and then making deductions on the basis of the probabilities 
;; estimated from tabulating the results of those experiments."
;; I don't get it ...
