w = 12
x = 3
y = 2
z = 4
y < z # no chaining, with True answer
z < y # no chaining, with False answer
x < z > y   # should be same as (3 < 4 and 4 > 2), True answer
x < z > w   # should be same as (3 < 4 and 4 > 12), False answer
(x < z) > y # should be same as (3 < 4 and True > 2)
y < x < z < w # 5-chain True answer
y < z < x < w # 5-chain False answer
y < z < x + 2 < w # 5-chain True answer, nested expression

w & (2 ** 4) != 0
