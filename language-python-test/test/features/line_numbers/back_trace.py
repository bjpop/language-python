def main():
    print (ackermann (3,4))

def ackermann (m, n):
    if m == 0: 
        # intentional error
        return n/0
    if m > 0 and n == 0:
        return ackermann (m-1, 1)
    if m > 0 and n > 0: 
        return ackermann (m-1, ackermann (m, n-1))

if __name__ == '__main__':
    main()
