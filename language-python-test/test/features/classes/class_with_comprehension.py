class C():
   l = [1,2,3]
   s = [ y + 1 for y in l]

print(C.l)
print(C.s)

# We treat it the same as below. The problem is that
# the variable l is not in scope in the definition of x()
# because of the unusual scope rules in class bodies
#class C():
#   l = [1,2,3]
#   def x():
#       result = []
#       for y in l:
#           result.append(y + 1)
#       return result
#   s = x() 
