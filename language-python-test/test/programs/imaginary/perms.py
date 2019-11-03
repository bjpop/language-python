def extract_items(list):
    result = []
    for index in range(0, len(list)):
        bottom = list[0:index]
        top = list[index+1:]
        item = list[index]
        result.append((item, bottom + top))
    return result

def perms(list):
    if list == []:
        return [[]]
    result = []
    for (item, rest) in extract_items(list):
        for p in perms(rest):
            result.append([item] + p)
    return result

for p in perms(list(range(4))):
    print(p)
