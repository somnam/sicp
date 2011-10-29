(define (say input) (display input) (newline))

;; 2.3 Symbolic data
;; Here we introduce the ability to work with arbitrary symbols as data.

;; 2.3.1 Quotation
;; In order to manipulate symbols we need the ability to quote a data object.
;; We want to construct a list (list a b). The common practice in natural
;; languages is to use quotation marks to indicate that a word or sendence
;; is to be treated literally as a string of characters. We can follow the
;; same practice to identify lists and symbols that are to be treated as
;; data objects, rather than expressions to be evaluated. Quoting in scheme
;; is done with a single quote at the beginning of the symbol. Thus, the
;; meaning of the single quote character is to quote the next object.

;; Now we can distinguish between symbols and their values:
;; (define quoted-list (list 'a 'b))
;; (display "Quoted list: ")
;; (display quoted-list)
;; (newline)

;; Quotation allows us to type in compound objects, using the conventional
;; printed representation for lists.
;; (define quoted-list '(a b c))
;; (display "Quoted list 2: ")
;; (display quoted-list)
;; (newline)
;; (display "Quoted list 2 cdr: ")
;; (display (cdr quoted-list))
;; (newline)

;; We can obtain the empty list by evaluating '().

;; One primitive used in manipulating symbols is eq? predicate. It takes two
;; symbols as arguments ad tests, whether they are the same. Actually 'eq?'
;; is one of equivalence predicates available in Scheme. It's the most
;; discriminating one (equal? and eqv? being the less discriminating ones).
;; Using this predicate we can define a procedure, which takes a symbol and
;; a list. If the symbol is not contained in the list, the procedure returns
;; false. Otherwise it returns a sublist, beginning with the first occurence.
(define (memq item list)
  (cond ((or (null? item) (null? list)) #f)
        ((eq? item (car list)) list)
        (else (memq item (cdr list)))))

;; (display "Does list contain: ")
;; (display (memq 'apple '(pear banana prune)))
;; (newline)
;; (display "Does list contain: ")
;; (display (memq 'apple '(x (apple sauce) y apple pear)))
;; (newline)

;; Exercise 2.53
;; (display "Examples: ")
;; (newline)
;; (display (list 'a 'b 'c))
;; (newline)
;; (display (list (list 'george)))
;; (newline)
;; (display (cdr '((x1 x2) (y1 y2))))
;; (newline)
;; (display (cadr '((x2 x2) (y1 y2))))
;; (newline)
;; (display (pair? (car '(a short list))))
;; (newline)
;; (display (memq 'red '((red shoes) (blue socks))))
;; (newline)
;; (display (memq 'red '(red shoes blue socks)))
;; (newline)

;; Exercise 2.54
(define (list-equal? list1 list2)
  ;; Lists have different length, nothing more to do
  (cond ((not (= (length list1) (length list2))) #f)
        ;; Both lists are empty or have same elements
        ((and (null? list1) (null? list2)) #t)
        ;; Elements on current position don't match
        ((not (eq? (car list1) (car list2))) #f)
        ;; Check next position
        (else (list-equal? (cdr list1) (cdr list2)))))

;;(display "Are lists equal? ")
;;(newline)
;;(display (list-equal? '(this is a list) '(this is a list)))
;;(newline)
;;(display (list-equal? '(this is a list) '(this (is a) list)))
;;(newline)
;;(display (list-equal? '(this is a list) '()))
;;(newline)
;;(display (list-equal? '() '()))
;;(newline)

;; 2.3.2 Symbolic differentiation

;; We will consider a very simple symbolic-differentiation program that handles
;; expressions built up only using the operations of addition and multiplication
;; with two arguments. Following rules apply:
;; - dc/dx = 0 for 'c' constant or a variable different than 'x'
;; - dx/dx = 1
;; - d(u+v)/dx = du/dx + dv/dx
;; - d(u*v)/dx = u*(dv/dx) + v*(du/dx)
;; Decomposing into smaller pieces will eventually produce pieces that are 
;; eihter constants or variables, whose derivatives will be equal to 0 or 1.

;; If we had means for representing algebraic expressions, we should be able to
;; tell whether an expression is a sum, product, constant or variable. Let us
;; assume, that we already have procedures to implement the following 
;; selectors, constructors and predicates:
;; (variable? e)          - is 'e' a variable
;; (same-variable? v1 v2) - are 'v1' and 'v2' the same variable
;; (sum? e)               - is 'e' a sum
;; (product? e)           - is 'e' a product
;; (addend e)             - get addend of sum 'e'
;; (augend e)             - auged of sum 'e'
;; (multiplier e)         - multiplier of the product 'e'
;; (multiplicand e)       - multiplicand of the product 'e'
;; (make-sum a1 a2)       - construct the sum of 'a1' and 'a2'
;; (make-product m1 m2)   - construct the product of 'm1' and 'm2'

;; Representing algebraic expressions
;; A straightforward choice is to represent algebraic expressions using
;; the paranthesized prefix notation, that Scheme uses for combinations.
;; This way we can represent a*x + b as (+ (* a x) b).

;; Helper methods

;; Variables are symbols - they are identified by 'symbol?' predicate:
(define (variable? x) (symbol? x))

;; Two variables are same, if the symbols representing them are equal:
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; Check if expression is equal to given number
(define (=number? expr num)
  (and (number? expr) (= expr num)))

;; Check if list contains only numbers
(define (numbers? e)
  (define (numbers-iter e res)
    (cond ((null? e) res)
          ((not (number? (car e))) #f)
          (else (numbers-iter (cdr e) res))))
  (numbers-iter e #t))
;; (display "numbers?() test cases: ")
;; (newline)
;; (display (numbers? '()))
;; (newline)
;; (display (numbers? '(1 2 3)))
;; (newline)
;; (display (numbers? '(1 2 x)))
;; (newline)
;; (display (numbers? '(x y z)))
;; (newline)
               
;; Push elem to end of list
(define (push li elem)
  (append li (cons elem '())))

;; Exercise 2.57 - handle arbitrary number of terms
(define (make-calc f s e)
  ;; Iterate over given elems
  (define (make-calc-iter res elems)
    (if (or (null? elems) (null? (car elems)))
        res
        (make-calc-iter (if (pair? res)
                           ;; Append element to result list
                           (push res (car elems))
                           ;; Accumulate element with result
                           (f res (car elems)))
                       (cdr elems))))

  ;; Start loop
  (make-calc-iter s e))

;; A sum is a list, whose first element is '+':
(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))
;; addend/augend belong to the list:
(define (addend e) (cadr e))
;; Add list of integers
(define (sum-integers e)
  (make-calc (lambda (i1 i2)
               (+ i1 i2))
             0
             e))
(define (augend e)
  (cond
    ;; One last auged remains
    ((< (length (cddr e)) 2) (caddr e))
    ;; List of values remains
    ((not (numbers? (cddr e))) (cons '+ (cddr e)))
    ;; List of numbers remains
    (else (sum-integers (cddr e)))))
;;  (if (< (length (cddr e)) 2)
;;      ;; One last auged remains
;;      (caddr e)
;;      ;; List of values remains
;;      ;; (cons '+ (cddr e))))
;;      (sum-integers (cddr e))))

;; Sums and products are constructed as lists:
(define (make-sum . e)
  (make-calc (lambda (a1 a2)
               (cond ((and (sum? a1)
                           (number? (addend a1))
                           (number? (augend a1)))
                      (make-sum (make-sum (addend a1)
                                          (augend a1))
                                a2))
                     ((and (sum? a2)
                           (number? (addend a2))
                           (number? (augend a2)))
                      (make-sum a1
                                (make-sum (addend a2)
                                          (augend a2))))
                     ((=number? a1 0) a2)
                     ((=number? a2 0) a1)
                     ((and (number? a1) (number? a2)) (+ a1 a2))
                     (else (list '+ a1 a2))))
             0
             e))

;; 'make-sum' test cases
;; (display "Sum arbitrary number of elements: ")
;; (newline)
;; (display (make-sum '()))
;; (newline)
;; (display (make-sum 'x))
;; (newline)
;; (display (make-sum 'x 'y))
;; (newline)
;; (display (make-sum 5 7))
;; (newline)
;; (display (make-sum 'x 'y 'z))
;; (newline)
;; (display (make-sum 5 6 7))
;; (newline)
;; (display (make-sum 5 'x 7))
;; (newline)
;; (display (make-sum 5 6 7 8 9 10))
;; (newline)
;; (display (make-sum '(+ 5 7) 8))
;; (newline)
;; (display (make-sum '(+ x 7) 8))
;; (newline)

;; A product is a list, whose first element is '*:
(define (product? e)
  (and (pair? e) (eq? (car e) '*)))
;; multiplier/multiplicand
(define (multiplier e) (cadr e))
;; Multiply list of integers
(define (multiply-integers e)
  (make-calc (lambda (i1 i2)
                   (* i1 i2))
                 1
                 e))
;;(define (multiplicand e) (caddr e))
(define (multiplicand e)
  (cond
    ;; One last multiplicand remains
    ((< (length (cddr e)) 2) (caddr e))
    ;; List of values remains
    ((not (numbers? e)) (cons '* (cddr e)))
    ;; List of numbers remains
    (else (multiply-integers (cddr e)))))
;;  (if (< (length (cddr e)) 2)
;;      ;; One last multiplicand remains
;;      (caddr e)
;;      ;; List of values remains
;;      ;; (cons '* (cddr e))))
;;      (multiply-integers (cddr e))))

;; multiplier/multiplicand test cases
;;(display "multiplier() and multiplicand() test cases: ")
;;(newline)
;;(display (multiplier '(* 1 2 3 4 5)))
;;(newline)
;;(display (multiplicand '(* 1 2 3 4 5)))
;;(newline)
;;(display (multiplier '(* x y)))
;;(newline)
;;(display (multiplicand '(* x y)))
;;(newline)
;;(display (multiplier (list '* '(* x y) 'z)))
;;(newline)
;;(display (multiplicand (list '* '(* x y) 'z)))
;;(newline)
;;(display (multiplier (list '* '(* x y) '(* x z))))
;;(newline)
;;(display (multiplicand (list '* '(* x y) '(* x z))))
;;(newline)

(define (make-product . e)
  (make-calc (lambda (m1 m2)
               (cond ((and (product? m1)
                           (number? (multiplier m1))
                           (number? (multiplicand m1)))
                      (make-product (make-product (multiplier m1)
                                                  (multiplicand m1))
                                    m2))
                     ((and (product? m2)
                           (number? (multiplier m2))
                           (number? (multiplicand m2)))
                      (make-product m1
                                    (make-product (multiplier m2)
                                                  (multiplicand m2))))
                     ((or (=number? m1 0) (=number? m2 0)) 0)
                     ((=number? m1 1) m2)
                     ((=number? m2 1) m1)
                     ((and (number? m1) (number? m2)) (* m1 m2))
                     (else (list '* m1 m2))))
             1
             e))

;; 'make-product' test cases
;; (display "Multiply arbitrary number of elements")
;; (newline)
;; (display (make-product '()))
;; (newline)
;; (display (make-product 'x))
;; (newline)
;; (display (make-product 'x 'y))
;; (newline)
;; (display (make-product 5 7))
;; (newline)
;; (display (make-product 'x 'y 'z))
;; (newline)
;; (display (make-product 5 6 7))
;; (newline)
;; (display (make-product 5 'x 7))
;; (newline)
;; (display (make-product '(* x y) '(* x z)))
;; (newline)

;; Exercise 2.56
;; Exponentation operation
(define (** b e)
  (define (**-iter res i)
    (if (or (= i 1) (= i 0))
        res
        (**-iter (* res b) (- i 1))))
  (**-iter b e))
(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))
(define (base e) (cadr e))
;; (define (exponent e) (caddr e))
(define (exponent-integers e)
  (make-calc (lambda (i1 i2)
                   (** i1 i2))
                 1
                 e))
(define (exponent e)
  (if (< (length (cddr e)) 2)
      ;; One last exponent remains
      (caddr e)
      ;; List of values remains
      (exponent-integers (cddr e))))
(define (make-exponentiation b . e)
  (make-calc (lambda (b e)
               (cond ((and (exponentiation? b)
                           (number? (base b))
                           (number? (exponent b)))
                      (make-exponentiation (make-exponentiation (base b)
                                                                (exponent b))
                                           e))
                     ((and (exponentiation? e)
                           (number? (base e))
                           (number? (exponent e)))
                      (make-exponentiation b
                                           (make-exponentiation (base e)
                                                                (exponent e))))
                     ((=number? e 0) 1)
                     ((=number? b 1) e)
                     ((and (number? b) (number? e)) (** b e))
                     (else (list '** b e))))
             b
             e))

;; 'make-exponentiation' test cases
;;(display "Exponentiate arbitrary number of elements: ")
;;(newline)
;;(display (make-exponentiation '()))
;;(newline)
;;(display (make-exponentiation 'x))
;;(newline)
;;(display (make-exponentiation 'x 'y))
;;(newline)
;;(display (make-exponentiation 2 3))
;;(newline)
;;(display (make-exponentiation 'x 'y 'z))
;;(newline)
;;(display (make-exponentiation 5 6 7))
;;(newline)
;;(display (make-exponentiation 5 'x 7))
;;(newline)

;; Using these and the primitive predicate 'number?' we can express the
;; differentiation rules as the following procedure:
(define (deriv expr var)
  (define (substract n1 n2)
    (if (and (number? n1) (number? n2))
        (- n1 n2)
        (list '- n1 n2)))

  (cond ((number? expr) 0)
        ;; - dc/dx = 0 for 'c' constant or a variable different than 'x'
        ;; - dx/dx = 1
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ;; - d(u+v)/dx = du/dx + dv/dx
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ;; - d(u*v)/dx = u*(dv/dx) + v*(du/dx)
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (multiplicand expr)
                         (deriv (multiplier expr) var))))
        ;; Exponentiation: d(u^n)/dx = n*u^(n-1)*(du/dx)
        ((exponentiation? expr)
         (make-product (exponent expr)
                       (make-product (make-exponentiation (base expr)
                                                          (substract (exponent expr) 1))
                                     (deriv (base expr)
                                            var))))
        (else (error "unknown expression type", expr))))

;; Test time
;;(display "deriv() test cases: ")
;;(newline)
;;(display (deriv '(+ x 3) 'x))
;;(newline)
;;(display (deriv '(* x y) 'x))
;;(newline)
;;(display (deriv '(* x y) 'x))
;;(newline)
;;(display (deriv '(* (* x y) (* x z)) 'x))
;;(newline)
;;(display (deriv '(* (* x y) (+ x 3)) 'x))
;;(newline)
;;(display (deriv '(** x 5) 'x))
;;(newline)
;;(display (deriv '(** x y) 'x))
;;(newline)
;;(display (deriv '(* x y (+ x 3)) 'x))
;;(newline)

;; Exercise 2.58 - modify the differentiation program so that it works with
;; ordinary mathematical notation


;; 2.3.3 Representing sets

;; Sets as unordered lists
;; One way to represent a set is a list of elements in which no element appears
;; more than once.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;;(display "element-of-set? test cases")
;;(newline)
;;(display (element-of-set? '() '()))
;;(newline)
;;(display (element-of-set? '() '(1)))
;;(newline)
;;(display (element-of-set? 1 '(1 2 3)))
;;(newline)
;;(display (element-of-set? '(1 2) '(1 2 3 4)))
;;(newline)

;; Join element into set
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; Compute intersection of two sets
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) 
         (cons (car set1) 
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Exercise 2.59 - compute union of two sets
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1)
                         (cons (car set1) set2)))))

;;(display "union-set test cases")
;;(newline)
;;(display (union-set '(1) '()))
;;(newline)
;;(display (union-set '(1 2 3) '(2 3 4 x y z)))
;;(newline)
;;(display (union-set '(1 2 x z) '(2 3 4 x y z)))
;;(newline)

;; Exercise 2.60 - TODO


;; Sets as ordered lists

;; One way to speed up the set operations is to change the set representation.
;; We allow only ordered lists as sets. A set of numbers will be represented by
;; listing its elements in an increasing order.
(define (sorted-set set)
  (if (sorted? set <)
      set
      (stable-sort set <)))

;;(display "sorted-set test cases")
;;(newline)
;;(display (sorted-set  '(9 7 4 2 3 5 6 98 1 8 63)))
;;(newline)
;;(display (sorted-set '(345 65 74234 2532 5476 6768 35 24 4246345645 74)))
;;(newline)

;; Our first representation allowed to represent the set {1,3,6,10} by listing
;; the elements in any order. Our new representation allows only the list
;; (1 3 6 10). Advantage of ordering shows up in element-of-set? function.
;; We no longer have to scan the entire set. If we reach for a element of set
;; that is larger than the item we are looking for, then we know that the item
;; is not in the set:
(define (element-of-sorted-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-sorted-set? x (cdr set)))))
;;(display "element-of-sorted-set? test cases")
;;(newline)
;;(display (element-of-sorted-set? 12 
;;                                 (sorted-set 
;;                                   '(6 8 4 2 3 8 9 55 78 22 657))))
;;(newline)

;; The average number of steps will be about n/2.
;; A more impressive speedup will be with intersection-set.
(define (intersection-sorted-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2) 
               (intersection-set (cdr set1) 
                                 set2))
              ((> x1 x2)
               (intersection-set set1
                                 (cdr set2)))))))

;; At each step we reduce the intersection problem to computing intersections
;; of smaller sets - by removing the first element from set1 or set2. The number
;; of steps required is at most the sum of set1 and set2 sizes, rather than the
;; product of their sizes. This is O(n) growth rather than O(n^2), which gives a
;; considerable speedup, even for sets of moderate size.

;; Exercise 2.61
(define (adjoin-sorted-set x set)
  (if (element-of-sorted-set? x set)
      ;; Element is contained within sorted set
      set
      ;; Append element and sort set
      (sorted-set (cons x set))))

;; Exercise 2.62 TODO


;; Sets as binary trees
;; A better solution is to arrange the set elements in the form of a binary tree.
;; Each node of the tree holds one element of set, called the 'entry' at that
;; node and a link to each two (possibly empty) nodes. The 'left' link points to
;; elements smaller than the one at the node and the 'right' link poins to elements
;; greater, than the one at the node. If the trees is 'balanced', than each of these
;; subtrees will be about half the size of the original. Thans to this we can reduce
;; the problem of searching a tree of size 'n' to a tree of size 'n/2'. Since the tree
;; is halved at each step, then we should expect that the number of steps needed to
;; search a tree of size 'n' grows as O(log n). 
;; We can represent trees by using lists. Each node will be a list of three items:
;; the entry at the node, the left subtree and the right subtree. An empty list will
;; indicate, that there is no subtree. We can describe this implementation by the 
;; following procedures:
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;;(display "make-tree() test cases")
;;(newline)
;;(display (make-tree 2
;;                    (make-tree 1 '() '())
;;                    (make-tree 3 '() '())))
;;(newline)
;;(display (make-tree 2 
;;                    (make-tree 1 '() '())
;;                    (make-tree (make-tree 3 '() '())
;;                               4 
;;                               '())))
;;(newline)
;;(define test-tree (make-tree 2 
;;                             (make-tree 1 '() '())
;;                             (make-tree (make-tree 3 '() '())
;;                                        4 
;;                                        '())))
;;(display (entry test-tree))
;;(newline)
;;(display (left-branch test-tree))
;;(newline)
;;(display (right-branch test-tree))
;;(newline)
;;(display (right-branch (right-branch test-tree)))
;;(newline)

;; No we can write element-of-tree? procedure using the strategy, described above:
(define (element-of-tree? x tree)
  (cond ((null? tree) #f)
        ((= x (entry tree)) #t)
        ((< x (entry tree))
         (element-of-tree? x (left-branch tree)))
        ((> x (entry tree))
         (element-of-tree? x (right-branch tree)))))

;;(display "element-of-tree? test cases")
;;(newline)
;;(define test-tree (make-tree 2 
;;                             (make-tree 1 '() '())
;;                             (make-tree 4 
;;                                        (make-tree 3 '() '())
;;                                        '())))
;;(display (element-of-tree? 3 test-tree))
;;(newline)
;;(display (element-of-tree? 4 test-tree))
;;(newline)

;; To adjoin a intem into a tree we compare x with the node entry to determine
;; whether x should be added to the left or right branch. If x is equal to the
;; entry we just return the node. If we are asked to adjoin x to an empty tree
;; we generate a tree that has x as the entry and empty left and right branches.
(define (adjoin-tree x tree)
  (cond ((null? tree) (make-tree x '() '()))
        ((= x (entry tree)) tree)
        ((< x (entry tree))
         (make-tree (entry tree)
                    (adjoin-tree x (left-branch tree))
                    (right-branch tree)))
        ((> x (entry tree))
         (make-tree (entry tree)
                    (left-branch tree)
                    (adjoin-tree x (right-branch tree))))))

;;(display "adjoin-tree() test cases")
;;(newline)
;;(define test-tree (make-tree 1 '() '()))
;;(display (adjoin-tree 2 test-tree))
;;(newline)
;;(display (adjoin-tree 3 test-tree))
;;(newline)
;;(display (adjoin-tree 4 test-tree))
;;(newline)
;;(display (adjoin-tree -1 test-tree))
;;(newline)
;;(display (adjoin-tree 5 (adjoin-tree 4 (adjoin-tree 3 (adjoin-tree 2 test-tree)))))
;;(newline)

;; Adjoining may produce an unbalanced tree as result. One way to solve this
;; problem is to create a procedure, that transforms an arbitrary tree into
;; a balanced tree with the same elements. We can perform this operation after
;; every few adjoin-tree operations to keep our set in balance.

;; Exercise 2.63
;; The following procedure converts a binary tree to a list:
(define (tree-to-list tree)
  (define (copy-to-list tree res)
    (if (null? tree)
        res
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          res)))))

  (copy-to-list tree '()))

;;(display "tree-to-list() test cases")
;;(newline)
;;(display (tree-to-list (make-tree 1 '() (make-tree 2 '() '()))))
;;(newline)
;;(display (tree-to-list (adjoin-tree 5 
;;                                    (adjoin-tree 4 
;;                                                 (adjoin-tree 3 
;;                                                              (adjoin-tree 2 
;;                                                                           (make-tree 1 
;;                                                                                      '() 
;;                                                                                      '())))))))
;;(newline)

;; Exercise 2.64
;; The following procedure converts an ordered list into a balanced binary tree.
(define (list-to-tree-old elements)
  (define (partial-tree elements n)
    (if (= n 0)
        ;; All list elements processed - return empty tree and remainig elements
        (cons '() elements)
        ;; Process 'n' list element
        ;; Get half of the number of list elements before n'th one
        (let ((left-size (quotient (- n 1) 2)))
          ;; Create partial tree for 'left-size' no. of elements
          (let ((left-result (partial-tree elements
                                           left-size)))
            ;; Get tree from result
            (let ((left-tree (car left-result))
                  ;; Get remainig list elements that aren't in result tree
                  (non-left-elements (cdr left-result))
                  ;; Get number of remaining elements
                  (right-size (- n (+ left-size 1))))
              ;; Get first element from list of remaining elements
              (let ((this-entry (car non-left-elements))
                    ;; Create partial tree for 'right-size' no of elements
                    (right-result (partial-tree (cdr non-left-elements)
                                                right-size)))
                ;; Get tree from result
                (let ((right-tree (car right-result))
                      ;; Get remainig elements
                      (remaining-elements (cdr right-result)))
                  ;; Build tree:
                  ;; - root node: first element from list of remainig elements
                  ;; - left node: left tree
                  ;; - right node: right tree
                  (cons (make-tree this-entry left-tree right-tree)
                        remaining-elements))))))))

  (car (partial-tree elements
                     (length elements))))

;; Code cleanup
(define (list-to-tree elements)
  (define (partial-tree elements n)
    (if (= n 0)
        ;; All list elements processed - return empty tree and remainig elements
        (cons '() elements)
        (let* (
               ;; Get half of the number of list elements before n'th one
               (left-size (quotient (- n 1) 2))
               ;; Create partial tree for 'left-size' part of elements
               (left-result (partial-tree elements left-size))
               ;; Get tree from result
               (left-tree (car left-result))
               ;; Get remainig list elements that aren't in result tree
               (remaining-elements (cdr left-result))
               ;; Get number of remaining elements
               (right-size (- n (+ left-size 1)))
               ;; Create partial tree for 'right-size' no of elements
               (right-result (partial-tree (cdr remaining-elements) right-size))
               ;; Root node of created tree:
               ;; first element from list of remaining elements
               (root-node (car remaining-elements))
               ;; Get tree from result
               (right-tree (car right-result))
               ;; Get remainig elements
               (remaining-elements (cdr right-result)))
          ;; Build tree:
          ;; - root node: first element from list of remainig elements
          ;; - left node: left tree
          ;; - right node: right tree
          (cons (make-tree root-node left-tree right-tree)
                remaining-elements))))

  (car (partial-tree elements
                     (length elements))))

;;(display "list-to-tree() test cases")
;;(newline)
;;(display (list-to-tree '(1 2 3 4 5 6 7)))
;;(newline)
;;(display (list-to-tree '(7 6 5 4 3 2 1)))
;;(newline)

;; Exercise 2.65 TODO

;; Sets and information retrieval
(define (lookup given-key set)
  (define (key cell)
    (if (null? cell) '() (car cell)))

  (define (val cell)
    (if (null? cell) '() (cadr cell)))

  (cond ((null? set) #f)
        ((equal? given-key (key (car set)))
         (val (car set)))
        (else (lookup given-key (cdr set)))))

;;(display "lookup() test cases")
;;(newline)
;;(define set-of-records (list '(a 1) '(b 2) '(c 3)))
;;(display (lookup 'b set-of-records))
;;(newline)
;;(display (lookup 'c set-of-records))
;;(newline)
;;(display (lookup 'd set-of-records))
;;(newline)
;;(display (lookup 'z set-of-records))
;;(newline)

;; Exercise 2.66 TODO

;; Huffman trees

;; Leaves of the tree are represented by a list, consisting of the following:
;; - 'leaf' symbol
;; - real symbol at the leaf
;; - weight
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? 'leaf (car object)))
(define (leaf-symbol object)
  (cadr object))
(define (leaf-weight object)
  (caddr object))

;;(display "leaf test cases")
;;(newline)
;;(define test-leaf (make-leaf 'A 10))
;;(display (leaf? test-leaf))
;;(newline)
;;(display (leaf-symbol test-leaf))
;;(newline)
;;(display (leaf-weight test-leaf))
;;(newline)

;; A general tree will be a list of left branch, right branch, a set of symbols
;; and a weight. The set of symbols will be a list of symbols. When we merge two
;; nodes, we obtaing the weight of the tree as a sum of the weights of the nodes
;; and the set of symbols as the union of the sets of symbols for the nodes.
;; Using the following selectors:
(define (huffman-symbols tree)
  (if (leaf? tree)
      (cons (leaf-symbol tree) '())
      (caddr tree)))
(define (huffman-weight tree)
  (if (leaf? tree)
      (leaf-weight tree)
      (cadddr tree)))
;; We can generate the tree:
(define (make-huffman-tree left right)
  (list left
        right
        (append (huffman-symbols left)
                (huffman-symbols right))
        (+ (huffman-weight left)
           (huffman-weight right))))

;;(display "make-huffman-tree test cases")
;;(newline)
;;(define test-left-leaf (make-leaf 'A 5))
;;(define test-right-leaf (make-leaf 'B 6))
;;(define test-huffman-tree (make-huffman-tree test-left-leaf
;;                                             test-right-leaf))
;;(display  (make-huffman-tree test-huffman-tree
;;                             (make-leaf 'C 3)))
;;(newline)

;; The decoding procedure
;; To decode a Huffman tree, we begin at the root node and use the successive
;; zeros and ones of the bit sequence to determine whether to move left or
;; right. Each time we come to a leaf, we generated a new symbol in the message,
;; at which point we start over from the root of the tree to find the next
;; symbol.
;; Decide on next move (left or right)
(define (huffman-left-branch tree) (car tree))
(define (huffman-right-branch tree) (cadr tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (huffman-left-branch branch))
        ((= bit 1) (huffman-right-branch branch))
        (else (display "Error! Bad bit: ")
              (display bit)
              (newline))))

;; The following procedure implements the decoding algorithm. As arguments It
;; takes a list of zeros and ones together with a Huffman tree.
(define (decode-huffman-tree bits tree)
  (define (decode-branch bits branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) branch)))
          (if (leaf? next-branch)
              (cons (leaf-symbol next-branch)
                    (decode-branch (cdr bits) tree))
              (decode-branch (cdr bits) next-branch)))))
  (decode-branch bits tree))

;;(say "decode-huffman-tree test cases")
;;(define test-bits '(0 0 0 1 1))
;;(define test-huffman-tree (make-huffman-tree (make-huffman-tree (make-leaf 'A 5)
;;                                                                (make-leaf 'B 6))
;;                                             (make-leaf 'C 3)))
;;(say (decode-huffman-tree test-bits
;;                          test-huffman-tree))
;;(define test-bits '(0 1 0 0 1 1))
;;(say (decode-huffman-tree test-bits
;;                          test-huffman-tree))
;;(define test-bits '(0 1 0 1 0 1 0 1 1))
;;(say (decode-huffman-tree test-bits
;;                          test-huffman-tree))

;; Exercise 2.67
;; DONE

;; Exercise 2.68
;; Write 'encode-symbol' procedure.

;; Check if symbol is supported by encoding
(define (is-symbol-defined symbol symbols)
  (cond ((null? symbols) #f)
        ((eq? symbol (car symbols)) #t)
        (else (is-symbol-defined symbol (cdr symbols)))))

;;(say "is-symbol-defined test cases")
;;(say (is-symbol-defined 'A '()))
;;(say (is-symbol-defined 'C '(A B C D)))
;;(say (is-symbol-defined 'D '(A B C)))

;; Encode given symbol
(define (encode-symbol symbol tree)
  ;; We reached a leaf, the symbol is encoded
  (cond ((leaf? tree) '())
        ;; Symbol is defned on left branch
        ((is-symbol-defined symbol
                            (huffman-symbols (huffman-left-branch tree)))
         ;; Add 0 to encode set
         (cons 0 (encode-symbol symbol
                                  (huffman-left-branch tree))))
        ;; Symbol is defined on right branch
        ((is-symbol-defined symbol
                            (huffman-symbols (huffman-right-branch tree)))
         ;; Add 1 to encode set
         (cons 1 (encode-symbol symbol
                                  (huffman-right-branch tree))))
        ;; Symbol not supported
        (else (display "Error! Symbol not supported: ")
              (display symbol)
              (newline))))

;;(say "encode-symbol test cases")
;;(define test-huffman-tree (make-huffman-tree (make-huffman-tree (make-leaf 'A 5)
;;                                                                (make-leaf 'B 6))
;;                                             (make-leaf 'C 3)))
;;(say (encode-symbol 'A test-huffman-tree))
;;(say (encode-symbol 'B test-huffman-tree))
;;(say (encode-symbol 'C test-huffman-tree))
;;(say (encode-symbol 'D test-huffman-tree))

;; Encode given message
(define (encode msg tree)
  (if (or (null? msg) (null? tree))
      '()
      (append (encode-symbol (car msg) tree)
              (encode (cdr msg) tree))))
;;(say "encode test cases")
;;(define test-huffman-tree (make-huffman-tree (make-huffman-tree (make-leaf 'A 5)
;;                                                                (make-leaf 'B 6))
;;                                             (make-leaf 'C 3)))
;;(say (encode '(A) test-huffman-tree))
;;(say (encode '(A C) test-huffman-tree))
;;(say (encode '(A) test-huffman-tree))
;;(say (encode '(A B C) test-huffman-tree))
;;(say (encode '(A B C C B A C B C) test-huffman-tree))
