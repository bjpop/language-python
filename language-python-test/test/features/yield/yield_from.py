def f():
    yield from [1,2,3]

x = f()
print(next(x))
print(next(x))
print(next(x))

try:
    next(x)
except StopIteration:
    print("StopIteration caught")
