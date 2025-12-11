DATA = open('day10/input.txt','r').read()

data = []
for line in DATA.splitlines():
    a,b = line.split(' ',1)[1].split('{')
    buttons = [[int(xx) for xx in x[1:-1].split(',')] for x in a.rstrip().split(' ')]
    joltage = [int(x) for x in b[:-1].split(',')]
    one_hot_buttons = []
    for button in buttons:
        one_hot_buttons.append([])
        for i in range(0,len(joltage)):
            one_hot_buttons[-1].append(int(i in button))
    data.append((one_hot_buttons,joltage))

from z3 import *

def solve(V,T):
    p = [Int(f'p{i}') for i in range(len(V))]
    s = Optimize()

    for x in p:
        s.add(x >= 0)

    for d in range(len(T)):
        s.add(sum(p[i] * V[i][d] for i in range(len(V))) == T[d])

    s.minimize(sum(p))
    s.check()
    m = s.model()

    return sum(m[p[i]].as_long() for i in range(len(V)))

print(sum(solve(V,T) for (V,T) in data))
