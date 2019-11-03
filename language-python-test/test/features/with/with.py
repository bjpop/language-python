class C(object):
    def __enter__(self):
        print("Entered context")
        return self
    def __exit__(self, ty, value, traceback):
        print("Exited context")
        print(ty)
        print(value)

try:
    with C() as c:
        print(type(c))
        1/0
except ZeroDivisionError:
    print("caught ZeroDivisionError")
