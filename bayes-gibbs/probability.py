"""
Based on http://code.google.com/p/aima-python/source/browse/trunk/probability.py
"""

import operator, random

def extend(s, var, val):
    s2 = dict(s)
    s2[var] = val
    return s2

def product(numbers): return reduce(operator.mul, numbers, 1)

def normalize(freqs):
    total = float(sum(freqs.values()))
    return dict((val, p / total) for val, p in freqs.items())

all_vars = []

class Variable:
    def __init__(self, parents, cpt):
        self.parents, self.cpt, self.children = parents, cpt, []
        for parent in parents: parent.children.append(self)
        all_vars.append(self)
    def gibbs_ask(self, e, N):
        assert self not in e, "Query variable must be distinct from evidence"
        Z = [var for var in all_vars if var not in e] # TODO: keep relevant vars only
        state = dict(e)
        for Zi in Z: state[Zi] = random.choice([True, False])
        counts = {True: 0, False: 0}
        for j in xrange(N):
            for Zi in Z:
                state[Zi] = Zi.markov_blanket_sample(state)
                counts[state[self]] += 1
        return normalize(counts)
    def markov_blanket_sample(self, e):
        Q = [self.p(xi, e) * product(Yj.p(ei[Yj], ei)
                                     for Yj in self.children)
             for xi in [False, True]
             for ei in [extend(e, self, xi)]]
        return random.uniform(0, sum(Q)) < Q[True]
    def p(self, value, e):
        ptrue = self.cpt[tuple(e[var] for var in self.parents)]
        return [1-ptrue, ptrue][value]


# Burglary example [Fig. 14.2]

T, F = True, False

burglary   = Variable((), {(): 0.001})
earthquake = Variable((), {(): 0.002})
alarm      = Variable((burglary, earthquake),
                      {(T, T): 0.95, (T, F): 0.94, (F, T): 0.29, (F, F): 0.001})
john_calls = Variable((alarm,), {(T,): 0.90, (F,): 0.05})
mary_calls = Variable((alarm,), {(T,): 0.70, (F,): 0.01})

def show_rounded(pdist, numfmt='%.3g'):
    return ', '.join([('%s: ' + numfmt) % (v, p)
                      for (v, p) in sorted(pdist.items())])

## random.seed(1017)
## show_rounded(burglary.gibbs_ask({john_calls:T, mary_calls:T}, 1000))
#. 'False: 0.738, True: 0.262'
