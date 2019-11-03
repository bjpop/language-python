# Monte Carlo calculation of Pi.
from random import (random, seed)
from math import sqrt

seed(1)

def approx_pi(samples):
    if samples <= 0:
        return 0

    in_circle = 0
    count = samples

    while count > 0:
        x = random()
        y = random()
        magnitude = sqrt(x*x + y*y)
        if magnitude <= 1:
            in_circle += 1
        count -= 1
    return 4.0 * in_circle / samples

s = 1
while s <= 1e6:
    print("{0}: {1}".format(s, approx_pi(s)))
    s *= 10
