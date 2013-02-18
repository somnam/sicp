;; Helpers
(define (say input) (display input) (newline))

;; 3.2 The environment model of evaluation
;; The substitution model of evaluation defines applying procedures to 
;; arguments as evaluating the body of the procedure with each formal parameter
;; replaced by the corresponding argument.
;; Once we introduce assignment into our language, such a definition is no longer
;; adequate. A variable no longer can be considered a name for a value. Rather
;; a variable must somehow designate a place in which values can be stored.
;; In our new model of evaluation these places will be maintained in structures
;; called environments.

;; An environment is a sequence of frames. Each frame is a table of bindings
;; which associate variable names with their corresponding values. A single
;; frame may contain at most one binding for each variable. Each frame has aslo
;; a pointer to its enclosing environment, unless the frame is considered global.
;; The value of the variable with respect to an environment is the value given
;; by the binding of the variable in the first frame in the environment that
;; contains that variable. If no such frame is found, then the variable is said
;; to be unbound.

;; The environment determines the context in which an expression should be
;; evaluated. An expression acquires a meaning only with respect to some
;; environment in which it is evaluated. The simple expression:
;; (+ 1 1)
;; depends on an understaning that one is operating in a context in which +
;; is the symbol for additon. In our model of evaluation we will always speak
;; of evaluating an expression with respect to some environment. To describe
;; interactions with the interpreter, we will suppose that there is a global
;; environment consisting of a single frame that includes values for symbols
;; associated with the primitive procedures e.g. the + symbol is bound in the
;; global environment to primitive additon procedure.

;; 3.2.1 Rules of evaluation
;; To evaluate a combination:
;; 1. Evalate the subexpressions of the combination.
;; 2. Apply the value of the operator subexpression to the values of the
;; operand subexpression.

;; In the environment model a procedure is always a pair consisting of some
;; code and a pointer to an environment. The procedures are created in one
;; way - by evaluating a lambda expression. This produces a procedure:
;; - which is obtained from the text of the lambda expression
;; - whose environment is the environment in which the lambda expression was
;;   evaluated
;; The following definition:
(define (square x) (* x x))
;; is just syntactic sugar for an underlying implicit lambda expression:
(define square (lambda  (x) (* x x)))
;; which evaluates (lambda  (x) (* x x)) and binds suquare to the resulting 
;; value. In general 'define' creates definitions by adding bindings to
;; frames (in this case the global frame).

;; Now we can describe how procedures are applied. To apply a procedure to
;; arguments, create a new environment containing a frame that binds the
;; parameters to the values of the arguments. The enclosing environment of this
;; frame is the environment specified by the procedure. Now within this new
;; environment evaluate the procedure body.
;; The environment model of procedure application can be sumarized by two rules:
;; - a procedure object is created by evaluating a lambda expression realtive
;;   to a given environment. The resulting object is a pair consisting of the
;;   text of the lambda expression and a pointer to the environment in which
;;   the procedure was created
;; - a procedure object is applied to a set of arguments by constructing a
;;   frame, binding the formal parameters of the procedure to the arguments of
;;   the call and then evaluating the body of the procedure in the context of
;;   the newly constructed environment
 
;; We also specify that using 'define' creates a binding in the current 
;; environment frame and assigns the indicated value to the symbol. Finally we
;; specify that using 'set!' locates the binding of the variable in the first
;; frame in the environment that contains a binding for the variable and changes
;; that binding to indicate the new value. If the variable is unbound, then set!
;; signals an error.

;; 3.2.2 Applying simple procedures
;; Lookup in book :).

;; 3.2.3 Frames as the repository of Local State
;; Procedures and assignment can be used to represent objects with local state.
;; Consider the withdrawal processor created by calling the procedure:
(define (make-withdrawal balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds.")))
;; It can be also written as:
(define make-withdrawal 
  (lambda (balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds."))))
;; The result of defining the make-withdrawal procedure produces a proceudure
;; object that contains a pointer to the global environment. The body of the
;; procedure is itself a lambda. The interesting part of the computation
;; happens when we apply the procedure make-withdrawal to an argument:
(define W1 (make-withdrawal 100))
;; We begin by setting up an environment E1 in which the formal parameter
;; 'balance' is bound to the argument 100. Within this environment we evaluate
;; the body of make-withdrawal - the lambda expression. This constructs a new
;; procedure object, whose code is specified by the lambda and whose environment
;; is E1, the environment in which the lambda was evaluated to produce the 
;; procedure. The resulting procedure object is bound to W1 - the global
;; environment, since the define itself is being evaluated in the global
;; environment. This procedure is a pair consisting of the lambda code and
;; a pointer to the E1 environment, in which balance is bound to value of 100.
;; Now we can analyze what happens when W1 is applied to an argument:
(say (W1 50))
;; We begin by constructing a frame in environment E1, in which the 'amount'
;; parameter is bound to the argument 50. The frame is constructed in E1 
;; because this is the environment that is specified by the W1 procedure object.
;; Within this environment we evaluate the body of the procedure. The expression
;; being evaluated references both amount and balance. Amount will be found in
;; the first frame of E1, while balance will be found by following the enclosing
;; environment pointer to E1.
;; When set! is executed the binding of balance in E1 is changed. At the end to
;; W1, balance is set to 50 and the frame, that contains balance is still 
;; pointed to by the procedure object W1. The frame that binds amount is no 
;; longer relevant, since the procedure call that constructed it (W1 50) has
;; terminated. There are no more pointers to that frame from other parts of the
;; environment. 
;; The next time W1 is called a new frame will be built, that binds amount and
;; whose enclosing environment is E1. We see that E1 serves as a place, that 
;; holds the local state variable for the procedure object W1.
;; When we create a second withdrawal object:
(define W2 (make-withdrawal 100))
;; creates the procedure object W2, that contains code and a pointer to the E2
;; environment. The environment contains a frame with its own local binding for
;; balance. W1 and W2 have the same code but reference state variables in 
;; different environments.

