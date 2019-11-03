def sorted_dict(dict):
    result = '{'
    items = []
    for key in sorted(dict):
        items.append((key, dict[key]))
    return result + ', '.join([repr(key) + ' : ' + repr(val) for (key, val) in items]) + '}'

class Meta(type):
    def __init__(self, class_name, bases, namespace):
        sorted_namespace = sorted_dict(namespace)
        print(class_name, bases, sorted_namespace) 
        #print(class_name, bases, namespace) 

class C(metaclass=Meta):
    pass

C()
