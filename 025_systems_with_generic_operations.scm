;; Helpers
(define (say input) (display input) (newline))

;; Get and put operations

;; Global list of available operations
(define op-table '())

;; Operation ((op type) proc), (op type) is key, proc is value
;; Operation constructor and selectors
(define (make-op op type proc)
  (cons (cons op type) proc))
(define (op-key op) (car op))
(define (op-proc op) (cdr op))

;;(say "operation test cases")
;;(say (make-op 'op-1 'type-1 (lambda (x) x)))
;;(define test-op (make-op 'op-1 'type-1 (lambda (x) x)))
;;(say (op-key test-op))
;;(say (op-proc test-op))
;;(say (make-op 'op-1 '(type-1 type-1) (lambda (x) x)))
;;(define test-op (make-op 'op-1 '(type-1 type-1) (lambda (x) x)))
;;(say (op-key test-op))
;;(say (op-proc test-op))

;; If operations list is empty - create with given entry
;; If operations list contains entry - don't add duplicate
;; If operations list doesn't contain entry - add it
(define (put-opt op type proc)
  (define (put-iter op-table-slice)
    (cond ((null? op-table-slice) 
           (set! op-table (cons (make-op op type proc)
                                op-table)))
          ((not (equal? (op-key (car op-table-slice)) 
                        (cons op type)))
           (put-iter (cdr op-table-slice)))))

  (put-iter op-table))

;;(say "put test cases")
;;(put-opt 'op-1 '(type-1 type-1) (lambda (x) x))
;;(say op-table)
;;(put-opt 'op-2 '(type-2 type-2) (lambda (x) x))
;;(say op-table)
;;(put-opt 'op-1 '(type-1 type-1) (lambda (x) x))
;;(say op-table)

(define (get-opt op type)
  (define (get-iter op-table-slice)
    (cond ((null? op-table-slice) #f)
          ((equal? (op-key (car op-table-slice))
                   (cons op type))
           (op-proc (car op-table-slice)))
          (else (get-iter (cdr op-table-slice)))))
  (get-iter op-table))

;;(say "get test cases")
;;(put-opt 'op-1 '(type-1 type-1) (lambda (x) x))
;;(put-opt 'op-2 '(type-2 type-2) (lambda (x) x))
;;(put-opt 'op-3 '(type-3 type-3) (lambda (x) x))
;;(say (get-opt 'op-1 '(type-1 type-1)))
;;(say (get-opt 'op-3 '(type-3 type-3)))
;;(say (get-opt 'op-2 '(type-3 type-3)))

;; Procedures for manipulating tagged data:
(define (set-tag tag content)
  (cons tag content))
;; Get tag from dataset
(define (get-tag datum)
  (if (pair? datum)
      (car datum)
      (say "Error! Bad tagged datum.")))
;; Get data from dataset
(define (get-content datum)
  (if (pair? datum)
      (cdr datum)
      (say "Error! Bad tagged datum.")))

;; 2.5 Systems with generic operations
;; Here we will learn how to define operations that are generic over different
;; operations and kinds of arguments. We will now use data-direct techniques
;; to construct a package of arithmetic operations that incorporates all the
;; arithmetic packages we have already constructed. The system will have following
;; abstraction barriers (laysers):

;; 1.
;; From the perspective of someone using numbers, there is a single procedure
;; 'add', that operates on whatever numbers are supplied. 'Add' is a part of
;; a generic interface that allows the separate:
;; - ordinary-arthmetic
;; - rational-arthmetic
;; - complex arthmetic
;; packages to be accessed uniformly by programs that use numbers.

;; 2.
;; Any individual arithmetic package may be accessed through generic procedures
;; (such as 'add-complex' or 'add-rat'), that combine packages designed for
;; different representations (such as rectangular and polar for complex numbers).
;; The structure of the system is additive, so that one can design the individual
;; arithmetic packages separately and combine them to produce a generic arithmetic
;; system.


;; 2.5.1 Generic arithmetic operations
;; The dask of designing generic arithmetic operations is analogous to designing
;; the generic complex number operations. We would like to have a generic addition
;; procedure 'add', that acts like:
;; - the ordinary primitive addition + on ordinary numbers
;; - 'add-rat' on rational numbers
;; - 'add-complex' on (guess what) complex numbers
;; We will attach a type tag to each kind of number and cause the generic procedure
;; to dispatch to an appropriate package according to the data type of its arguments:
(define (apply-generic op . args)
  (let ((tags (map get-tag args)))
    (let ((proc (get-opt op tags)))
      (if proc
          (apply proc (map get-content args))
          (say "Error! No method for given types.")))))

;; The generic arithmetic procedures are as follows:
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; We begin by installing a package for ordinary numbers. We will tag these with
;; the symbol 'scheme-number'. Since the operations take two arguments, they are
;; installed in the table keyed by list (scheme-number scheme-number):
(define (install-scheme-number-arithmetic-package)
  (define (tag x) (set-tag 'scheme-number x))

  (put-opt 'add '(scheme-number scheme-number)
           (lambda (x y) (tag (+ x y))))
  (put-opt 'sub '(scheme-number scheme-number)
           (lambda (x y) (tag (- x y))))
  (put-opt 'mul '(scheme-number scheme-number)
           (lambda (x y) (tag (* x y))))
  (put-opt 'div '(scheme-number scheme-number)
           (lambda (x y) (tag (/ x y))))

  (put-opt 'make 'scheme-number
           (lambda (x) (tag x)))
  #t)

(define (make-scheme-numer n)
  ((get-opt 'make 'scheme-number) n))

;; Rational arithmetic package:
(define (install-rational-arithmetic-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x)
                 (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x)
                 (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (tag x) (set-tag 'rational x))

  (put-opt 'add '(rational rational)
           (lambda (x y) (tag (add-rat x y))))
  (put-opt 'sub '(rational rational)
           (lambda (x y) (tag (sub-rat x y))))
  (put-opt 'mul '(rational rational)
           (lambda (x y) (tag (mul-rat x y))))
  (put-opt 'div '(rational rational)
           (lambda (x y) (tag (div-rat x y))))

  (put-opt 'make 'rational
           (lambda (n d) (tag (make-rat n d))))

  #t)

(define (make-rational n d)
  ((get-opt 'make 'rational) n d))

;; Handling complex numbers:
;; Rectangular package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (set-tag 'rectangular x))
  (put-opt 'real-part '(rectangular) real-part)
  (put-opt 'imag-part '(rectangular) imag-part)
  (put-opt 'magnitude '(rectangular) magnitude)
  (put-opt 'angle '(rectangular) angle)
  (put-opt 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put-opt 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  #t)

;; Polar package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (set-tag 'polar x))
  (put-opt 'real-part '(polar) real-part)
  (put-opt 'imag-part '(polar) imag-part)
  (put-opt 'magnitude '(polar) magnitude)
  (put-opt 'angle '(polar) angle)
  (put-opt 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put-opt 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  #t)

(define (install-complex-arithmetic-package)
  ;; Imported from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get-opt 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get-opt 'make-from-mag-ang 'polar) a r))

  ;; Internal stuff
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; Interface
  (define (tag z) (set-tag 'complex z))
  (put-opt 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put-opt 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put-opt 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put-opt 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put-opt 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put-opt 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  #t)

(define (make-complex-from-real-imag x y)
  ((get-opt 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get-opt 'make-from-mag-ang 'complex) r a))

;; Here we have a two-level tag system. The outer tag 'complex is used to direct
;; the number to the complex package. Once within the complex package, the next
;; tag ('rectangular or 'polar) is used to direct the number to the correct
;; package.

;; Install ALL the packages
(install-scheme-number-arithmetic-package)
(install-rational-arithmetic-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-arithmetic-package)

;; Test cases
;;
;;(define scheme-number-t1 (make-scheme-numer 5))
;;(define scheme-number-t2 (make-scheme-numer 10))
;;(say (get-content (add scheme-number-t1 scheme-number-t2)))
;;(say (get-content (sub scheme-number-t1 scheme-number-t2)))
;;(say (get-content (mul scheme-number-t1 scheme-number-t2)))
;;(say (get-content (div scheme-number-t1 scheme-number-t2)))
;;
;;(define rational-number-t1 (make-rational 5 7))
;;(define rational-number-t2 (make-rational 7 4))
;;(say (get-content (add rational-number-t1 rational-number-t2)))
;;(say (get-content (sub rational-number-t1 rational-number-t2)))
;;(say (get-content (mul rational-number-t1 rational-number-t2)))
;;(say (get-content (div rational-number-t1 rational-number-t2)))
