def g(f):
    return lambda x: f(x) + 1

@g
def f(x):
    return x + 1

if __name__ == '__main__':
    print(f(1))
