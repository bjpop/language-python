# test construction of booleans by calling the bool class

# call with zero arguments

bool()

# call with bool arguments

bool(True)
bool(False)

# call with int arguments

bool(1)
bool(-1)

# call with float arguments

bool(1.0)
bool(-1.0)

# call with complex arguments

bool(1j)
bool(-1j)

# call with tuple arguments

bool(())     # 0-tuple
bool((1,))
bool(((),())) # 2-tuple

# call with list arguments

bool([])
bool([1])

# call with string arguments

bool("")
bool("s")

# call with dict arguments

bool({})
bool({1:1})
bool({1:1, 2:2})

# call with set arguments

bool(set())
bool(set([1]))
bool(set([1,2]))

# call with an object argument

bool(object())

# call with a user defined class and object

class Foo(object): pass

bool(Foo)
bool(Foo())
