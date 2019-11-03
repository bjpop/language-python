# We once had a bug with this code.
# The issue is due to the alphabetical order of the arguments to
# ray_trace (x is after recurse_cutoff in lexicographic ordering).
# We didn't generate the varnames vector correctly, because we put the
# variable names in arbitrary order, instead of the order that they are
# listed as arguments. 
def ray_trace(x, recurse_cutoff=0):
    return x, recurse_cutoff 

print(ray_trace(1, recurse_cutoff=5))
