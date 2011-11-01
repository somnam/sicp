(define (say input) (display input) (newline))

;; 2.4 Multiple representations of abstract data
;; In this section we will learn how to cope with data, that may be represented
;; in different ways by different parts of the program. This requires generic
;; procedures - procedures, that operate on data, which can be represented in
;; more than one way. Our data objects will have type tags - include explicit
;; information about how they will be processed.

;; 2.4.1
;; We will develop a system that performs arithmetic operations on complex
;; numbers. First we will discuss two representations for complex numbers as
;; ordered pairs: 
;; - rectangular form (real and imaginary part) 
;; - polar form (magnitude and angle)
;; These two representations can be shown to coexist in a single system through
;; the use of type tags and generic operations.

;; Complex numbers can be represented as ordered pairs. The complex number
;; z = x + i*y, where i^2 = 1
;; can be though of as a point in the plane, whose real coordinate is x and
;; imaginary coordinate is y. Addition of complex numbers in this representation
;; reduces to addition of coordinates:
;; Real-part(z1+z2)      = Real-part(z1) + Real-part(z2)
;; Imaginary-part(z1+z2) = Imaginary-part(z1) + Imaginary-part(z2)
;; When multiplying complex numbers, it is more natural to think in terms of
;; the polar form - as magnitude (r) and angle (A). The product of two complex
;; numbers is the vector obtained by stretching one complex number by the length
;; of the other and then rotating it through the angle of the other:
;; Magnitude(z1*z2) = Magnitude(z1) * Magnitude(z2)
;; Angie(z1*z2)     = Angle(z1) * Angle(z2)
;; Thus there are two representations of complex numbers, which are appropriate
;; for different operations. The principle of data abstraction suggests, that
;; all operations for manipulating complex numbers should be available regardless
;; of which representation is used by the computer. It is often useful to be able
;; to determine the real part of a complex number that is specified by polar
;; coordinates.

;; Using these constructors and selectors we can implement arithmetic on complex
;; numbers using the 'abstract data' specified by the constructors and selectors.
;; First we must choose a representation in terms of primitive numbers and list
;; structures. Both rectangular and polar forms can be represented as a pair,
;; either (real part, imaginary part) or (magnitude, angle).
;; We can add and substract complex numbers in terms of real and imaginary parts
;; while multiplying and dividing complex numbers in terms of magnitudes and angles.
;; But we are not limited to one representation. We can use either the rectangular or
;; polar representation. The abstraction barrier formed by selectors and constructors
;; permits us to defer to the last possible moment the choice of a concrete 
;; representation for our data objects and retain maximum flexibility in our design. 
;; This is called the principle of 'least commitment'. We can event maintain the
;; ambiguity of representation even after designing selectors and constructors.
;; Both representations can be included in a single system, but we need a way to
;; distinguish data in polar and rectangular form. A straightforward way to accomplish
;; this is to include a type tag - symbol 'polar or 'rectangular as a part of each
;; complex number. We can use the tag to decide which selectors to apply.

;; Procedures for manipulating tagged data are as follows:
;; Attach tag to dataset
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
;; Using these procedures we can define predicates which recognize polar and
;; rectangular representations:
(define (rectangular? z)
  (eq? (get-tag z) 'rectangular))
(define (polar? z)
  (eq? (get-tag z) 'polar))

;; Assume that operations on complex numbers are implemented in terms of four
;; selectors: real-part, imag-part, magnitude, angle. Also assume that we have
;; two procedures for constructing complex numers: 
;; make-from-real-imag - returns complex number with real and imaginary parts
;; make-from-mag-ang   - returns complex number with magnitude and angle
;; These procedures have a property, that for any complex number 'z' produce
;; complex numbers, that are equal to z.
;; Given following trigonometric relations:
;; x = r * cos(A)
;; y = r * sin(A)
;; r = sqrt(x^2 + y^2)
;; A = arctan(y, x)
;; Rectangular representation ('rect, x y):
(define (square x) (* x x))
(define (real-part-rect z) (car z))
(define (imag-part-rect z) (cdr z))
(define (magnitude-rect z)
  (sqrt (+ (square (real-part-rect z))
           (square (imag-part-rect z)))))
(define (angle-rect z)
  (atan (imag-part-rect z)
        (real-part-rect z)))
(define (make-from-real-imag-rect x y)
  (set-tag 'rect (cons x y)))
(define (make-from-mag-ang-rect r a)
  (set-tag 'rect
           (cons (* r (cos a)) (* r (sin a)))))

;; Polar representation ('polar, r, A):
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (make-from-real-imag-polar x y)
  (set-tag 'polar
           (cons (sqrt (+ (square x) (square y))A)
                 (atan y x))))
(define (make-from-mag-ang-polar r a)
  (set-tag 'polar (cons r a)))

;; Generic selectors:
(define (imag-num-gen-selector z func-rect func-polar err)
  (cond ((rectangular? z) (func-rect (get-content z)))
        ((polar? z) (func-polar (get-content z)))
        (else (say err))))
(define (real-part-gen z)
  (imag-num-gen-selector z 
                         real-part-rect 
                         real-part-polar
                        "Error! Unknown type for real part."))
(define (imag-part-gen z)
  (imag-num-gen-selector z 
                         imag-part-rect 
                         imag-part-polar 
                        "Error! Unknown type for imaginary part."))
(define (magnitude-gen z)
  (imag-num-gen-selector z
                         magnitude-rect
                         magnitude-polar
                         "Error! Unknown type for magnitude part."))
(define (angle-gen z)
  (imag-num-gen-selector z
                         angle-rect
                         angle-polar
                         "Error! Unknown type for angle part."))

;; Constructing complex numbers is very straightforward. We construct rectangular
;; numbers when we have real and imaginary parts and polar numbers when we have
;; magnitudes and angles:
(define (make-from-real-imag x y)
  (make-from-real-imag-rect x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; To implement arithmetic operations we call generic procedures:
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part-gen z1) (real-part-gen z2))
                       (+ (imag-part-gen z1) (imag-part-gen z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part-gen z1) (real-part-gen z2))
                       (- (imag-part-gen z1) (imag-part-gen z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude-gen z1) (magnitude-gen z2))
                     (+ (angle-gen z1) (angle-gen z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude-gen z2) (magnitude-gen z2))
                     (- (angle-gen z2) (angle-gen z2))))

;; The complex number system has been divided into three parts:
;; - arithmetic operations using generic selectors
;; - polar implementation
;; - rectangular implementation
;; Since all data objects are tagged with their types, the selectors operate on
;; them in a generic manner.

;; 2.4.3 Data-directed programming and addivity
;; The general strategy of checking the type of datum and calling an appropriate
;; procedure is called dispatching on type. But implementing the dispatch as done
;; earlier with complex numbers has two weaknesses:
;; - generic interface procedures (real-part, imag-part etc.) must know about all 
;;   the different representations
;; - even though the individual representations can be designed separately, we must
;;   guarantee that no two procedures in the entire system have the same name
;; The issue underlying both of these weaknesses is that the technique for implementing
;; generic interfaces is not additive. We need to modularize the system desing by using
;; the data-direct programming technique. When dealing with a set of operations that
;; are common to a set of different types we are in fact looking at a two-dimensional
;; table with possible operations on one axis and possible types on the other axis. 
;; The entries in the table are procedures, that implement each operation for each
;; type of argument present. 

;; In this method we can implement the interface as a single procedure that looks
;; up the combination of the operation name and argument type in the table to find
;; the correct procedure to apply. To add a new representation package to the system
;; we need only to add new entries in the table. The key idea of data-directed
;; programming is to handle generic operations in programs by dealing explicitly
;; with operation-and-type tables. The required dispatching is organized on type
;; by having each operation take care of its own dispatching. This decomposes the
;; operation-and-type table into rows, with each generic operation procedure
;; representing a row in the table.

;; An alternative implementation strategy is to decompose the table into columns
;; and instead of using 'inteligent operations' that dispatch on data types, we
;; use 'inteligent data objects' that dispatch on operation names. A rectangular
;; number can be a data object, which is represented as a procedure. The procedure
;; takes as input the required operation name and performs it. E. g.
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle)
           (atan y x))
          (else (say "Error! Unknown operation."))))
  dispatch)
;; The value returned by this procedure is itself a procedure.
;; The corresponding apply-generic procedure now feeds the operations name 
;; to the data object and lets the object do the work:
(define (apply-generic op arg) (arg op))

;; This style of programming is called message passing. The data object is an
;; entity that receives the requested operation name as a message.
