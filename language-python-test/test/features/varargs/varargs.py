def show_sorted_dict(d):
    result = '{'
    items = ["{} : {}".format(k, d[k]) for k in sorted(d.keys())]
    result += ', '.join(items)
    return result + '}'

def f(*x):
    return x

print(f())
print(f(1))
print(f(1,2))
print(f(1,2,3))
print(f((1,2,3)))
print(f(*(1,2,3)))

def g(x, *y):
    return (x, y)

print(g(1))
print(g(1,2))
print(g(1,2,3))
print(g((1,2,3)))

def f(**x):
    return x

d = f(x=1)
print(show_sorted_dict(d))
d = f(x=1,y=2)
print(show_sorted_dict(d))
d = f(**{'a':1,'b':2,'c':3})
print(show_sorted_dict(d))

def g(x, **y):
    return (x, y)

print(g(1))
x, d = g(1,z=2)
print("({}, {})".format(x, show_sorted_dict(d)))
x, d = g(1,a=2,b=3)
print("({}, {})".format(x, show_sorted_dict(d)))

def f(a, b, *c, **d):
    return (a, b, c, d)

print(f(1, 2))
print(f(1, 2, 3))
x, y, z, d = f(1, 2, 3, z=4)
print("({}, {}, {}, {})".format(x, y, z, show_sorted_dict(d)))
