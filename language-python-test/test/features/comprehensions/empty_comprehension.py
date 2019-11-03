# empty list
[x for x in []]

# empty set
{x for x in []}

# empty dict
{x:x for x in []}

# empty gen
gen = (x for x in [])
for item in gen:
   print(item)
