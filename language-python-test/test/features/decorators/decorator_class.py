def f(c):
    print("Class %s was decorated" % str(c))
    return c


@f
class C(object):
    pass

C()
