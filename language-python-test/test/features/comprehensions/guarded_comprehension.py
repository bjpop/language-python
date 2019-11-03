list = [1,2,3]

# list
[x for x in list if x < 1]
[x for x in list if x > 3]
[x for x in list if x == 2]
[x for x in list if x == x]
[x for x in list if True]
[x for x in list if False]

# set: this depends on the order that items in sets are printed.
{x for x in list if x < 1}
{x for x in list if x > 3}
{x for x in list if x == 2}
{x for x in list if x == x}
{x for x in list if True}
{x for x in list if False}

# dict: this depends on the order that items in dicts are printed.
{x:x for x in list if x < 1}
{x:x for x in list if x > 3}
{x:x for x in list if x == 2}
{x:x for x in list if x == x}
{x:x for x in list if True}
{x:x for x in list if False}

# dict: this depends on the order that items in dicts are printed.
(x for x in list if x < 1)
(x for x in list if x > 3)
(x for x in list if x == 2)
(x for x in list if x == x)
(x for x in list if True)
(x for x in list if False)
