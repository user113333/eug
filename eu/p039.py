from math import *

def isPerfectSquare(x):
    return floor(sqrt(x))**2 == x

mx = 1000
triplets = []

for a in range(1, mx):
    for b in range(1, a + 1):
        c2 = a**2 + b**2
        if isPerfectSquare(c2) and a + b + isqrt(c2) <= mx:
            triplets.append((a, b, isqrt(c2)))

pc = [0] * 1001

for (a, b, c) in triplets:
    pc[a + b + c] += 1

top = 0
top_res = 0
for i in range(1001):
    if pc[i] > top:
        top = pc[i]
        top_res = i

print(top_res)
