# 1.2 Procedures and processes they generate

# 1.2.1 Linear recursion and iteration

# Recursive factorial function:
# - n! can be computed by multiplying n and (n-1)!
# - 1! is equal to 1
def fact_rec(n):
    res = 1
    if n != 1:
        res = n * fact_rec(n - 1)
    return res

print(fact_rec(10))

# n! can be computed by multiplying 1*2*3* ... *(n-1)*n
# We can first multipy 1*2, then the result by 3,
# then the result by 4 etc.
def fact_rec2(n):
    def fact_iter(product, counter, max):
        return product if counter > max else fact_iter((product * counter),
                                                       (counter + 1),
                                                       max)
    return fact_iter(1, 1, n)

print(fact_rec2(10))

# inc() and dec() - increment and decrement value
def inc(x): return (x + 1)
def dec(x): return (x - 1)

# Add two positive integers
# Variant 1 - recursive procedure
def add1(a, b):
    res = b
    if a != 0:
        res = inc(add1(dec(a), b))
    return res

print(add1(13,44))

# Variant 2 - iterative procedure
def add2(a, b):
    res = b
    if a != 0:
        res = add2(dec(a), inc(b))
    return res

print(add2(33,11))

# Exercise 1.10

# Ackerman's function is defined as follows:
def A(x, y):
    res = None
    if y == 0:
        res = 0
    elif x == 0:
        res = 2 * y
    elif y == 1:
        res = 2
    else:
        res = A((x -1), A(x, (y - 1)))

    return res

print(A(1,10))
print(A(2,4))
print(A(3,3))

# 1.2.2 Tree recursion

# An example of tree recursion is computing Fibonacchi numbers, where
# each number is the sum of two preceding.

# A recursive procedure for computing the Fibonacchi numbers looks so
def fib_rec(n):
    res = n
    if not n in (1,2):
        res = fib_rec(n -1) + fib_rec(n - 2)
    return res

print(fib_rec(5))

# To compute (fib 5) we compute (fib 4) and (fib 3). To compute (fib 4)
# we compute (fib 3) and (fib 2) etc. This does so much redundant computation.

# Iterative version of Fibonacchi bases on applying repeatedly
# the given simultaneous transformations:
# a = a + b
# b = a
def fib_iter(n):
    a,b = 0,1
    while n >= 0:
        a,b = a + b, a
        n  -= 1
    return a

print(fib_iter(100))

# Number of ways in which we can exchange a money amount 'a' given 'n' kinds
# of coins. Recursive approach
def count_change(amount, coin_types=6):
    # Take number of coin types and return the denomination of first kind
    # switch in python : mapping
    first_denomination = {
        1 : 1,
        2 : 5,
        3 : 10,
        4 : 25,
        5 : 50,
        6 : 100,
    }

    make_change = None
    # There is no way to make change when
    # - value to change is a negative number
    # - there are no types of coins to make change from
    if amount < 0 or coin_types == 0:
        make_change = 0
    # There is only one way to make change
    elif amount == 0:
        make_change = 1
    # The number of ways to change amount of money with
    # given kinds of coins equals sum of:
    # - number of ways to change 'a' using all but the first kind of coin
    else:
        make_change = (
            count_change(amount, (coin_types - 1)) +
            count_change((amount - first_denomination.get(coin_types)), coin_types)
        )
    return make_change

print(count_change(100))

# Exercise 1.11

# Function f is defined by the rule:
# n < 3  : f(n) = n
# n >= 3 : f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3)

# Recursive verision
def fun1(n):
    out = n
    if n >= 3:
        out = fun1(n - 1) + 2*fun1(n - 2) + 3*fun1(n - 3)
    return out

print(fun1(10))

# Iterative verision
# f(0) = 0
# f(1) = 1
# f(2) = 2
# f(3) = f(2) + 2*f(1) + 3*f(0)
# f(4) = f(3) + 2*f(2) + 3*f(1)
# etc.
def fun2(n):
    out = n

    if n >= 3:
        # Generic step-value count
        step_value = lambda x,y,z: x + 2*y + 3*z

        # Loop start conditions
        fn_1, fn_2, fn_3 = 2, 1, 0

        for counter in range(3, n+1):
            fn_1, fn_2, fn_3 = step_value(fn_1, fn_2, fn_3), fn_1, fn_2

        out = fn_1

    return out

print(fun2(10))

Exercise 1.12

Pascals triangle:
- the numbers on the edge of the triangle are all 1
- each number inside the triangle is the sum of two numbers above it

# Recursive version
# f(1) = (1)
# f(2) = (1,1)
# f(3) = (1,f(2)[1-1]+f(2)[1],1)
# f(4) = (1,f(3)[1-1]+f(3)[1],f(3)[2-1]+f(3)[2],1)
def pascal_triangle(n, view=1):
    # Generic cell value count
    def cell_value(row, i):
        return row[i - 1] + row[i]

    # Loop start conditions
    rows = [[1]]

    for row_i in range(1, n):
        # Build current row
        row = [1]
        for row_j in range(1, row_i):
            row.append(cell_value(rows[-1], row_j))
        row.append(1)

        # Append row to list
        rows.append(row)

    # View results ?
    if view:
        for row in rows:
            print(row)

    return rows

def pascal_triangle2(n, view=1):
    # Loop start conditions
    prev_row = [1]

    for row_i in range(1, n):
        # Build current row
        row = [1]
        for row_j in range(1, row_i):
            row.append(prev_row[row_j - 1] + prev_row[row_j])
        row.append(1)

        # Append row to list
        prev_row = row

    # View results ?
    if view:
        print(row)

    return row

pascal_triangle2(10, view=0)
