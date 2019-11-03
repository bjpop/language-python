emptyListComp = [x for x in []]
singleListComp = [x for x in [1]]

emptySetComp = {x for x in []}
singleSetComp = {x for x in [1]}

# list nested with list
[x for x in emptyListComp]
[x for x in singleListComp]

# set nested with set
{x for x in emptySetComp}
{x for x in singleSetComp}

# list nested with set
[x for x in emptySetComp]
[x for x in singleSetComp]
