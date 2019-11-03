g = ( x + y for x in [1,2,3] if x > 1 for y in [4,5,6] if y < 6 )

print(type(g))

for x in g:
   print(x)
