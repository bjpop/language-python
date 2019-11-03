# should really have Bool cases in here too

# int `op` float, float `op` int

1 - 2.0
1.0 - 2

2 + 3.0
2.0 + 3

4 * 5.0
4.0 * 5

6 / 3.0
6.0 / 3

# int `op` complex, complex `op` int

1 - 2j
1j - 2

2 + 3j
2j + 3

4 * 5j
4j * 5

6 / 3j
6j / 3

# float `op` complex, complex `op` float

1.0 - 2j
1j - 2.0

2.0 + 3j
2j + 3.0

4.0 * 5j
4j * 5.0

6.0 / 3j
6j / 3.0
