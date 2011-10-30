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
;; make-from-rect-form  - returns complex number with real and imaginary parts
;; make-from-polar-form - returns complex number with magnitude and angle
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
