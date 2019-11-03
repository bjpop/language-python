def power_set(s):
    if len(s) == 0:
        return [[]]
    rest = power_set(s[1:])
    result = []
    for item in rest:
        result.append(item)
        result.append([s[0]] + item)
    return result

def power_set_iter(s):
    result = []
    # iterate over all numbers between 0 and 2^N - 1
    for n in range(2**(len(s))):
        # build new subset based on the binary encoding of n
        # and the index of the item in the list
        index = 0
        subset = [] 
        for item in s:
            # test is n has the bit at index set to 1
            if n & (2 ** index) != 0:
                # include the item in the current subset
                subset.append(item)
            # update the index for the next item
            index += 1
        # include the subset in the final result
        result.append(subset)
    return result

for s in power_set(range(5)):
    print(s)

for s in power_set_iter(range(5)):
    print(s)
