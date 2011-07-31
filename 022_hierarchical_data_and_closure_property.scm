;; 2.2.1 Representing sequences

;; The ability to construct pairs whose elements are pairs is essence of
;; list structure's importance as a representational tool. We refer to this
;; ability as the closure property of 'cons'. An operation for combining
;; data objects satisfies the closure property if results of combining elements
;; with that operation can themselves be combined with using the same operation.
;; Closures permit the use of hierarchical data structures - structures made of
;; parts, that are themselves made up of parts.

;; One of useful structures, that can be built with pairs is a sequence.
;; An ordered collection of data objects, that internally are chains of parts.
;; The 'car' of each pair is the corresponding item in the chain, while 'cdr'
;; points to the next pair in the chain. The 'cdr' of the last pair points to
;; distinguished value, that is not a pair (like the variable 'nil' or '()).

(define nested-pair (cons 1
                          (cons 2
                                (cons 3 '()))))

;; Such a sequence of pairs is called a list. Scheme provides a primitive
;; called 'list', that helps in constructing lists.
(define one-to-four (list 1 2 3))
(display "List: ")
(display one-to-four)
(newline)
(display "First element of list pair: ")
(display (car one-to-four))
(newline)
(display "Second element of list pair: ")
(display (cdr one-to-four))
(newline)
(display "First element of second list pair: ")
(display (car (cdr one-to-four)))
(newline)

;; List operations
;; Retrieve the n-th element of a list.
(define (list-ref l n)
  (if (= n 0)
      ;; For n = 0 return 'car' of current pair
      (car l)
      ;; Otherwise get the next element and process it
      (list-ref (cdr l) (- n 1))))

(define squares (list 1 4 9 16 25))
(display "List element at [3]: ")
(display (list-ref squares 3))
(newline)

;; We often cdr down the whole list. Scheme includes a predicate 'null?' to
;; test, whether its argument is the empty list. The procedure 'length'
;; which returns the number of elements in list uses this predicate.
(define (list-len l)
  (define (list-len-iter l n)
    ;; We reached the last pair in list
    (if (null? l)
        n
        (list-len-iter (cdr l) (+ n 1))))
  (list-len-iter l 0))

(display "List length: ")
(display (list-len squares))
(newline)

;; The 'append' procedure combines elements of two lists into one.
;; - if list1 is empty then return list2
(define (list-app l1 l2)
  (if (null? l1)
      ;; l1 is empty, return the second list argument
      l2
      ;; (cons (car l1) (append (cdr l1) l2))))
      (append l1 l2)))

(define cubes (list 1 8 27 64 125))
(display "Append two lists: ")
(define squares-and-cubes (list-app squares cubes))
(display squares-and-cubes)
(newline)
(display "First element of combined list: ")
(display (car squares-and-cubes))
(newline)
(display "Second element of combined list: ")
(display (cdr squares-and-cubes))
(newline)

;; Exercise 2.17
;; Define procedure, that returns list, which contains only the last
;; element of list.
(define (list-last-pair l)
  (if (null? l)
    ;; Got empty list, return it
    l
    ;; Get last element of list and return it as list
    (list (list-ref l (- (list-len l) 1)))))

(display "Last element of list: ")
(display (list-last-pair cubes))
(newline)
(display "Last element of empty list: ")
(display (list-last-pair '()))
(newline)

;; Exercise 2.18
;; Define procedure, that returns a list in reversed order.
(define (list-rev l)
  ;; Calculate length only once
  (let ((l-len (list-len l)))
    ;; Loop through list
    (define (list-rev-iter out-list n)
      (if (= n l-len)
        out-list
        (list-rev-iter (cons (list-ref l n)
                             out-list)
                       (+ n 1))))
    (list-rev-iter '() 0)))

(display "Reverse list 1: ")
(display (list-rev squares))
(newline)
(display "Reverse list 2: ")
(display (list-rev cubes))
(newline)

;; Exercise 2.19
;; TODO

;; Exercise 2.20
;; Procedures, such as '+', '*' and 'list' take arbitrary numbers of arguments.
;; One way to define such procedures is to use 'define' with dotted-tail
;; notation. A parameter list that has a dot before the last parameter name
;; indicates, that when the procedure is called, the final parameters value
;; will be a list of any remaining arguments.
;; Write a procedure 'same-parity', that takes one or more integers and
;; returns a list of all the arguments, that have same even-odd parity.
(define (same-parity f . r)
  ;; Store parity of first element
  (let ((rem (remainder f 2)))
    ;; Append element to output list, if has same parity
    (define (same-parity-append out-list elem)
      (if (eqv? (remainder elem 2) rem)
          (cons elem out-list)
          out-list))
    ;; Iterative process for building output
    (define (same-parity-iter out-list n)
      (if (< n 0)
          out-list
          (same-parity-iter (same-parity-append out-list
                                                (list-ref r n))
                            (- n 1))))
    (same-parity-iter '() (- (list-len r)
                             1))))

;; Try it!
(display "Same parity as 2: ")
(display (same-parity 2 4 5 6 7 8 9 13 15 17 18 22 29 33))
(newline)
(display "Same parity as 13: ")
(display (same-parity 13 4 5 6 7 8 9 15 17 18 22 29 33))
(newline)

;; Map
;; Example - scale each number in list by a given factor.
(define (list-scale items factor)
  (define (list-scale-iter out-list n)
    (if (< n 0)
      out-list
      (list-scale-iter (cons (* (list-ref items n)
                                factor)
                             out-list)
                       (- n 1))))
  (list-scale-iter '() (- (list-len items) 
                          1)))

(display "Factor list by 4: ")
(display (list-scale (list 2 3 4 5 6 7 8 9) 4))
(newline)

;; In general:
(define (map f l)
  (define (map-iter out-list n)
    (if (< n 0)
      out-list
      (map-iter (cons (f (list-ref l n)) 
                      out-list)
                (- n 1))))
  (map-iter '() (- (list-len l) 
                   1)))

(display "Map lambda to list: ")
(display (map (lambda (x) (* x x x))
              (list 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
(newline)

;; This procedure helps to establish an abstraction barrier, that isolates
;; the implementation of procedures, that transform lists from the detail
;; of how elements in list are extracted and combined.

;; Exercise 2.21
;; Done above.

;; Exercise 2.22
;; Done above

;; Exercise 2.23
;; for-each, just like map applies a procedure to list of arguments,
;; but the values are not used at all. The value returned by for-each
;; can be an arbitrary value (like number of times the procedure has
;; been applied)
(define (for-each l f)
  (let ((len (- (list-len l) 1)))
    ;; Iterate over input list
    (define (for-each-iter n)
      ;; Just return no. of operatins
      (cond ((> n len) len)
            ;; Process current value with given function and call self
            (else (f (list-ref l n))
                  (for-each-iter (+ n 1)))))
    ;; Start loop
    (for-each-iter 0)))

(display "for-each example: ")
(newline)
(for-each (list 3 5 7 9 13 15 17 19 21 23 25)
          (lambda (x) (display (* x x x)) (display ", ")))
(newline)

;; 2.2.2 Hierarchical structures
;; A way of thinking of sequences, whose elements are themselves sequences
;; is a tree. Elements of the sequence are branches of the tree and elements
;; which themselves are sequences, are subtrees.

;; To count the length (total number of leaves) of a tree, we use recursion.
;; Count-leaves works by the following rules:
;; - for empty list returns 0
;; - for leaf returns 1
;; - for a tree T returns count-leaves of (car T) + (cdr T)
(define (count-leaves T)
  (cond ((null? T) 0)
        ;; The 'pair?' predicate tests whether its argument is a pair
        ((not (pair? T)) 1)
        (else (+ (count-leaves (car T))
                 (count-leaves (cdr T))))))

;; Define tree that will be used in Exercises
(define T (list 1 2 3 (list 2 3 4 (list 3 4 5 6 (list 7 8 9) 10))))

;; Exercise

;; Describe value of current leaf
(define (describe-leaf l)
  (display "Tree leaf of value: ")
  (display l)
  (newline))

;; Iterate over each element of tree (or sub-tree) and display it's value.
;; (define (tree-walk T)
;;   ;; Count leaves only once
;;   (let ((leng (- (list-len T) 1)))
;;     (define (tree-walk-iter T i)
;;       ;; Walk until last leaf reached
;;       (if (<= i leng)
;;           (let ((leaf (list-ref T i)))
;;             ;; Describe leaf or iterate sub-tree
;;             (cond ((list? leaf) (tree-walk leaf))
;;                   ((not (null? leaf)) (describe-leaf leaf)))
;;             ;; Proceed to next leaf
;;             (tree-walk-iter T (+ i 1)))))
;;     ;; Start from first leaf
;;     (tree-walk-iter T 0)))
(define (tree-walk T)
  (map (lambda (leaf)
         (if (pair? leaf)
             (tree-walk leaf)
             (describe-leaf leaf)))
       (reverse T)))

;; Walk through example tree
(display "Walk through example tree: ")
(newline)
(tree-walk T)

(display "Walk through example tree2: ")
(newline)
(tree-walk (list (list 1 2) 3 4))

;; Exercise 2.24
(display "Length of tree T: ")
(newline)
(display (count-leaves T))
(newline)

;; Exercise 2.25
(display "CDR to 7: ")
(display (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))))
(newline)

(display "CDR to 7: ")
(display (car (car (list (list 7)))))
(newline)

(display "CDR to 7: ")
(define l1 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l1)))))))))))))
(newline)

;; Exercise 2.26
(define l1 (list 1 2 3))
(define l2 (list 4 5 6))

(display "Cons l1 l2: ")
(display (cons l1 l2))
(newline)

(display "List l1 l2: ")
(display (list l1 l2))
(newline)

;; Exercise 2.27
;; deep-reverse procedure, used to reverse all sub-lists in a tree
(define (tree-rev T)
  (if (not (list? T))
      T
      (list-rev (map tree-rev T))))
(display "Reverse tree: ")
(display (tree-rev T))
(newline)
(display "Reverse tree 2: ")
(display (tree-rev (list (list 1 2) 3 4)))
(newline)

;; Exercise 2.28
;; fringe procedure, that takes a tree as an argument and returns a list
;; whose elements are all leaves of the tree, arranged in left-to-right
;; order
(define (fringe T)
  ;; Leaves list will be stored here
  (let ((out-list (list)))
    ;; - if T is leaf, append to result list
    ;; - if T is list, iterate over each element
    (define (fringe-append T)
      (if (not (pair? T))
        ;; This is stupid, a single element can't be appended
        ;; to end of a list ...
        (set! out-list
              (cons T out-list))
        (for-each T fringe-append)))
    ;; Run loop
    (fringe-append T)
    ;; Return result (must be reversed because of using 'cons')
    (reverse out-list)))

(display "Fringe a tree: ")
(display (fringe T))
(newline)

(define T2 (list (list 1 2) (list 3 4)))
(display "Fringe another tree: ")
(display (fringe (list T2 T2)))
(newline)

;; Exercise 2.29
;; TODO

;; Mapping over trees
;; 'map' together with recursion is a powerful abstraction for dealing
;; with trees. For instance 'scale-tree' procedure takes as arguments
;; a numeric factor and a tree, whose leaves are numbers. It returns
;; a tree of the same shape, where each number is multiplied by the factor.
(define (scale-tree T f)
  (cond ((null? T) '())
        ((not (pair? T)) (f T))
        (else (cons (scale-tree (car T) f)
                    (scale-tree (cdr T) f)))))

;; Try it!
(display "Scale given tree: ")
(display (scale-tree T (lambda (x) (* x 10))))
(newline)

;; Another way to implement scale-tree is to regard the tree as a sequence
;; of sub trees and use 'map'. We map over the sequence, scaling each sub-tree
;; in turn and return the list of results.
(define (scale-tree-map T f)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-map sub-tree f)
             (f sub-tree)))
       T))

(display "Scale a tree by 4: ")
(display (scale-tree-map T (lambda (x) (* x 4))))
(newline)

;; Exercise 2.30
;; square-tree
(define (square-tree T)
  (scale-tree T (lambda (x) (* x x))))

(display "Square a tree: ")
(display (square-tree T))
(newline)

;; Exercise 2.31
;; Already done with scale-tree.

;; Exercise 2.32
;; Generate a set of subsets of a set.
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (display x))
                          rest)))))

;; (display "All subsets of a set: ")
;; (display (subsets (list 1 2 3)))
;; (newline)

;; 2.2.3 Sequences as conventional interfaces
;; Conventional interfaces abstract details of data representations and
;; preserve the flexibility to experiment with alternative representations.
;; Ability to formulate analogous operations for working with compound
;; data depends crucially on the style in which we manipulate our data
;; structures.
;; For example cosider this procedure, which takes a tree as an
;; argument and computes sum of squares of leaves, that are odd:
(define (sum-odd-squares T)
  (cond ((null? T) 0)
        ((not (pair? T)) (if (odd? T)
                             (* T T)
                             0))
        (else (+ (sum-odd-squares (car T))
                 (sum-odd-squares (cdr T))))))

(display "Sum odd squares: ")
(display (sum-odd-squares T))
(newline)

;; Count n'th fib number
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        a
        (fib-iter b (+ a b) (- count 1))))
  (fib-iter 0 1 n))

;; On the surface, this procedure is very different from following one,
;; which constructs a list of all even Fibonacci numbers:
(define (even-fibs n)
  ;; Iterate loop
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))

  ;; Start loop
  (next 0))

(display "List of even fib numbers: ")
(display (even-fibs 100))
(newline)

;; A more abstract description of the two computations reveals a great deal
;; of similarity.
;; The first program:
;; - enumerates the leaves of a tree
;; - filters them, selecting odd ones
;; - squares each of filtered values
;; - acumulates the result using +, starting with 0
;; The second program:
;; - enumerates the integers from 0 to n
;; - computes the Fibonacci number for each integer
;; - filters them, selecting odd ones
;; - acumulates the result using 'cons', starting with '()

;; These procedures can be conceptualized in terms of signals
;; flowing through a cascade of stages. Each stage implements
;; part of the program plan.
;; In sum-odd-squares we begin with an enumerator, that generates
;; a signal consisting of leaves of a given tree.
;; This signal is passed through a filter, which leaves only the
;; odd elements.
;; The resulting signal is passed through a map, which applies the
;; 'square' procedure to each element. The output of map is then fed to an
;; 'accumulator', which combines the elements using '+'. starting from an
;; initial value of '0'.
;; [enumerate]->[filter]->[map]->[accumulate]

;; Unfortunately, the two definitilions fail to exhibit this signal-flow
;; structure. In sum-odd-squares we find, that the enumeration is implemented
;; partly by the 'null?' and 'pair?' tests. Similarly, the accumulation is
;; found partly in the tests and partly in recursive addition. There re no 
;; distinct parts that correspond to the elements of mentioned signal-flow
;; description.

;; If we could organize our programs to make the signal-flow structure maifest
;; in the procedures we write, this would increase the conceptual clarity of
;; resulting code.

;; The key to organize our programs is to focus on the 'signals' that flow from
;; one stage in the process to the next. If we represent those signals as lists,
;; then we can use list operations to implement the processing at each of the
;; stages.

;; Mapping stages of the signal flow can be implemented with the 'map'
;; procedure:
(map (lambda (x) (* x x)) (list 1 2 3 4))

;; Filtering a sequence to select only those elements that satisfy the given
;; predicate can be acomplished by:
(define (filter predicate seq)
  ;; Nothing to check - return empty list
  (cond ((null? seq) '())
        ;; Element satisfies predicate - append to output
        ;; and continue with next sequence element
        ((predicate (car seq)) (cons (car seq)
                                     (filter predicate (cdr seq))))
        ;; Element doesn't satisfy predicate
        ;; just continue with next element
        (else (filter predicate (cdr seq)))))

(display "Filter odd numbers from list: ")
(display (filter odd? (list 1 2 3 4 5 6 7 8)))
(newline)

;; Accumulations can be implemented with:
(define (accumulate op init seq)
  (if (null? seq)
      ;; Return initial value
      init
      ;; Apply operator
      (op (car seq)
          (accumulate op init (cdr seq)))))

(display "Add elements of list, starting with 0: ")
(display (accumulate + 0 (list 1 2 3 4 5 6 7 8 9 10)))
(newline)

;; Enumerating the sequence of elements to be processed is the last
;; step.For even-fibs we can generate a sequence of integers in a given
;; range, which we can do as follows:
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(display "Interval {2,12): ")
(display (enumerate-interval 2 20))
(newline)

;; Now we can reformulate even-fibs in terms of signal-flow diagram:
;; - enumerate integers from 0 to n
;; - generate the Fibonacci number for each of these integers
;; - filter resulting sequnece to keep only the even elements
;; - accumulate the results into a list
(define (even-fibs-sig n)
  ;; Create a range of (0, n)
  (let ((interval (enumerate-interval 0 n)))
    ;; Make list of fibonacci numbers for given range
    (let ((fibonacci (map fib interval)))
      ;; Filter out non-even numbers
      (let ((filtered (filter even? fibonacci)))
        ;; Is this really needed ? The result is already a list.
        ;; (accumulate cons '() filtered)))))
        filtered))))

(display "List even fibs with signal processing: ")
(display (even-fibs-sig 100))
(newline)

;; To enumerate the leaves of a tree we can use:
(define (enumerate-tree T)
  (cond ((null? T) '())
        ((not (pair? T)) (list T))
        (else (append (enumerate-tree (car T))
                      (enumerate-tree (cdr T))))))

(display "Enumerate a tree: ")
(display (enumerate-tree (list 1 2 (list 3 4 (list 5 6 (list 7 8 (list 9 10)))))))
(newline)

;; For sum-odd-squares we:
;; - enumerate the sequence of leaves of the tree
;; - filter this to keep only odd numbers
;; - square each element
;; - sum the results
(define (sum-odd-squares T)
  (let ((enumerated (enumerate-tree T)))
    (let ((filtered (filter odd? enumerated)))
      (let ((squared (map (lambda (x) (* x x)) filtered)))
        (accumulate + 0 squared)))))

(display "Sum odd squares: ")
(display (sum-odd-squares T))
(newline)

;; Modular construction is a powerful strategy for controlling complexity
;; in engineering design. It helps us make program designs that are modular.
;; We can encourage modular design by providing a library of standard
;; components together with a conventional interface for connecting the
;; components in a flexible way.

;; Exercise 2.23
;; Fill in the missing expressions:
(define (map-acc p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(display "Map by accumulate: ")
(display (map-acc (lambda (x) (* x x)) (list 1 2 3 4 5)))
(newline)

(define (append-acc seq1 seq2)
  (accumulate cons seq2 seq1))

(display "Append by accumulate: ")
(display (append-acc (list 1 2 3 4) (list 5 6 7 8)))
(newline)

(define (length-acc sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(display "Length by accumulate: ")
(display (length-acc (enumerate-interval 1 100)))
(newline)

;; Exercise 2.34
;; Evaluating a plynomial at a given value of x can be formulated as an
;; accumulation. We can use the Horner's rule which for the given plynomial:
;; (an*x^n + a(n-1)*x^(n-1) + ... + a1*x + a0)
;; structures the computation in x as:
;; a0 + x * ( a1 + x * ( a2 + x * ( a3 + ... + x * (an-2 + x * (an-1 + an*x)))))
;; Fill in the following template to produce a procedure, that evaluates
;; a polynomial using Horners rule.
(define (horner-eval x coefficients)
  (accumulate (lambda (coeff terms)
                      (+ coeff (* x terms)))
              0
              coefficients))
;; For example, to compute 1 + 3x + 5x^3 + x^5 at x = 2 you would evaluate:
;; 1 + 3x + 0x^2 + 5x^3 + 0x^4 + 1x^5
;; 1 + x * (3 + x * (0 + x * (5 + x * (0 + x))))
(display "Evaluate polynomial for x=2: ")
(display (horner-eval 2 (list 1 3 0 5 0 1)))
(newline)

;; Exercise 2.35
;; Redefine count-leaves from section 2.2.2 as an accumulation
;; Fill in the missing expressions:
;; (define (count-leaves T)
;;   (accumulate ?? ?? (map ?? ??)))

;; Exercise 2.36
;; The procedure accumulate-n is similar to accumulate, but it takes as the
;; third argument a sequence of sequences, which are all assumed to have
;; the same number of elements. It applies the given procedure to combine all
;; first elements of sequence, all second elements of sequence, all third
;; elements of sequence etc. and return a sequence of results.
;; Fill in the missing expressions:
;; (define (accumulate-n op init seqs)
;;   (if (null? (car seqs))
;;       '()
;;       (cons (accumulate op init ??)
;;             (accumulate op init ??))))

;; Exercise 2.37
;; TODO

;; Exercise 2.38
;; TOOD

;; Exercise 2.39
;; TODO

;; Nested mappings
;; We can extend the sequence (list) paradingm to include many computatons that
;; commonly expres using nested loops. Consider the following problem:
;; given a positive integer 'n' find all ordered pairs of distinct positive
;; integers 'i' and 'j', where:
;; o 1 < j < i < n
;; o i + j is prime
;; If n=6, then the pairs are following:
;; (2,1), (3,2), (4,1), (4,3), (5,2), (6,1), (6,5)
;; A way to organize this:
;; - genereate sequence of all ordered pairs of positive integers, less or
;;   equal to n
;; - filter to select those pairs, whose sum is prime
;; - for each remaining pair, produce a triple (i,j, i + j)

;; Generating sequence of pairs
;; For each integer i <= n, enumerate integers j < i, and for each such i and j
;; generate pair (i,j). In terms of sequence operations, we map along
;; the sequence (enumerate-interval 1 n). For each i in this sequence, whe
;; generate the pair (list i j). This gives us a sequence of pairs for each i.
;; The combination of mapping and accumulating with 'append'is so common in
;; this sort of program, that we will isolate it as a separate procedure:
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
;; Using this procedure we can generate the pairs:
(define (generate-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(display "Generate pairs up to 5: ")
(display (generate-pairs 5))
(newline)

;; Now filter this sequence of pairs to find those, whose sum is prime.
;; The filter predicate is called for each element of the sequence.
;; It must extract the integers from pair and test them:
(define (prime-sum? pair)
  ;; Check if integer is prime
  (define (prime? n)
    (define (smallest-divisor n)
      ;; Check if n divides m
      (define (divides? n m)
        (= (remainder m n) 0))

      ;; Find divisor for given 'n' value
      (define (find-divisor n test)
        (cond ((> (* test test) n) n)
              ((divides? test n) test)
              (else (find-divisor n (+ test 1)))))

      (find-divisor n 2))

    ;; divisor of 'n' is equal 'n'
    ;; this means that 'n' is a prime value
    (= n (smallest-divisor n)))

  ;; 'cadr' - cdr of car (second element of first elements pair)
  (prime? (+ (car pair) (cadr pair))))

;; Finally generate the sequence of results. by creating a triple, consisting
;; of the two elements of pair along with their sum:
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; Combine steps:
(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum?
                             (generate-pairs n))))

(display "Generate sum pairs up to 5: ")
(display (prime-sum-pairs 5))
(newline)

;; Nested mappings are also useful for sequences other than those
;; that enumerate intervals. Let's say, for generating all the
;; permutations of a set S, that is all the ways of ordering the
;; items in the set:
;; - for each item in x in S recursively generate the sequence of permutations
;;   of S - x and join 'x' to the front of each one
;; - this yelds for each x a set of permutations of S, that begin with x
;; - combining these sequences for all x gives all the permutations of S
(define (permutations S)
  ;; 'remove' procedure
  (define (remove item seq)
    (filter (lambda (x) (not (= x item)))
            seq))

  ;; Empty set?
  (if (null? S)
      ;; Empty set
      (list '())
      ;; Process each element in set
      (flatmap (lambda (x)
                 ;; Append x to beginning of permutation
                 (map (lambda (p) (cons x p))
                      ;; Create list of permutatons for set reduced by 'x'
                      (permutations (remove x S))))
               S)))

(display "All permutations of 4 element set: ")
(display (permutations (list 3 4 5 6)))
(newline)

;; Exercise 2.40
;; 
