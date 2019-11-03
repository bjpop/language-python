verse_pattern = \
"""{0} bottles of beer on the wall,
{0} bottles of beer,
if you take one down and pass it around,
there'll be {1} bottles of beer on the wall"""

def verse(n):
    return verse_pattern.format(n, n-1)

def song(n):
    for n in range(n,0,-1):
        print(verse(n))
    print("0 bottles of beer on the wall")

song(99) 
