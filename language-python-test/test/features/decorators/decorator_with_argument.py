def g(x):
    print("x = %d" % x)
    def h(f):
        print("called on %s" % f.__name__)
        return f
    return h

@g(3)
def f():
    pass

f()
