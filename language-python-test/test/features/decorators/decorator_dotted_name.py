import decorator_function as d

# this is defined in decorator_function:
# def g(f):
#     return lambda x: f(x) + 1

@d.g
def f(x):
    return x + 1

print(f(1))

