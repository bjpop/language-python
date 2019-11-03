# singleton tuple <- singleton tuple
x, = 0,
# singleton tuple <- singleton list
x, = [-1]
# binary tuple <- binary tuple
x,y = 1,2
# binary tuple swap
x,y = y,x
# ternary tuple <- ternary tuple
x,y,z = 3,4,5
# singleton list <- singleton list
[x] = [42]
# singleton list <- singleton tuple
[x] = 43,
# binary list <- binary list
[x,y] = [6,7]
# binary list <- binary tuple
[x,y] = [44,45]
# binary tuple (parens) <- binary list
(x,y) = [7,8]
# binary tuple <- result of function call
(x,y) = (lambda: (9,10))()
# nested binary tuple (parens) <- nested binary tuple (parens)
((x,y),z) = ((11,12),13)
# nested binary tuple <- nested binary tuple
(x,y),z = (14,15),16
