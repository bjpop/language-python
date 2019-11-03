class C(object):
    def __enter__(self):
        print("Entered context")
        return self
    def __exit__(self, ty, value, traceback):
        print("Exited context")

with C() as c, C() as d:
    print(type(c))
    print(type(d))
