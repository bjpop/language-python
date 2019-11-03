class C(Exception):
    pass

try:
    raise C 
except C:
    print("caught exception")
