def f():
   yield 3
   yield 5

print(type(f))

x = f()

print(type(x))

print(next(x))

print(next(x))

try:
    next(x)
except StopIteration:
    print("StopIteration caught")
