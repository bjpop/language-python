# delete a global variable
x = 12
del x
try:
   print(x)
except NameError:
   print("Caught NameError")

# delete a subscript
xs = [1,2,3]
del xs[0]
print(xs)

# delete an attribute
class C(): pass
c = C()
c.x = 12
del c.x
try:
   print(c.x)
except AttributeError:
   print("Caught AttributeError")

# delete a local variable
def f():
    x = 12
    del x
    try:
        print(x) 
    except UnboundLocalError:
        print("Caught UnboundLocalError")
f()

# delete a slice
xs = [1,2,3,4,5]
del xs[1:3]
print(xs)
