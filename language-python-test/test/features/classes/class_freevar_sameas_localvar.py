def f():
    y = 'from f'
    class C():
        y = 'from C'
        def g(self):
            nonlocal y
            print(y)
    C().g()

f()
