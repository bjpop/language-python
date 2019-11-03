# Test the treatment of list assignments in nested scopes.
# This was previously a bug when we didn't compute the scope
# of subscript assignments correctly.
def f():
    a = [0]
    def inc():
        a[0] += 1
    inc()
    return a

print(f())
