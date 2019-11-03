def g(f):
    print("g called on %s" % str(f.__name__))
    return f

def h(f):
    print("h called on %s" % str(f.__name__))
    return f

@g
@h
def f():
    print("f was called")

f()
