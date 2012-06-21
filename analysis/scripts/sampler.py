#!/usr/bin/python

import numpy as np
import scipy.stats.distributions as dists
from collections import Counter

def flip(weight=0.5):
    return np.random.uniform(0, 1) <= weight

def multiply_dists(d1, d2):
    d3 = {}
    for k in d1.keys():
        d3[k] = d1[k]*d2[k]
    return d3

def mass(d):
    return sum(d.values())

def sample_dist(d):
    pt = d.get(True, 0.0)
    pf = d.get(False, 0.0)
    if flip(pt/(pt+pf)):
        return True
    else:
        return False

def print_dist(model):
    N = 100000
    for (k, v) in Counter([model() for m in range(N)]).items():
        print k, v/float(N)


# --------------------------------------------------------------------
# No reuse

def original_model():
    A = flip(.5)
    if A:
        B = flip(1/3.0)
        return B
    else:
        C = flip(.2)
        return not(C)

def model_1():
    if flip(.5*(2/3.0)):
        return False
    else:
        Amarg = { True : .5 }
        Aunexplored = { True : 1/3.0 }
        Amargux = multiply_dists(Amarg, Aunexplored)
        m1 = mass(Amargux)
        m2 = 1-mass(Amarg)
        p_marg = m1/(m1+m2)
        p_int = m2/(m1+m2)
        if flip(p_marg):
            A = sample_dist(Amargux)
            diverged = False
        else:
            A = False
            diverged = True
        if A:
            B = True
            return B
        else:
            C = flip(.2)
            return not(C)

def model_2():
    m1 = .5*(2/3.0)
    m2 = .25*(4/5.0)
    if flip(m1 + m2):
        if flip(m1/(m1+m2)):
            return False
        else:
            return True
    else:
        Amarg = { True : .5, False : .25 }
        Aunexplored = { True : 1/3.0, False : 1/5.0 }
        Amargux = multiply_dists(Amarg, Aunexplored)
        m1 = mass(Amargux)
        m2 = 1-mass(Amarg)
        p_marg = m1/(m1+m2)
        p_int = m2/(m1+m2)
        if flip(p_marg):
            A = sample_dist(Amargux)
            diverged = False
        else:
            A = False
            diverged = True
        if A:
            B = True
            return B
        else:
            if diverged:
                C = flip(.2)
            else:
                C = True
            return not(C)


# --------------------------------------------------------------------
# Reuse

def choose_marg_prob(marg, unexplored):
    margux = multiply_dists(marg, unexplored)
    m1 = mass(margux)
    m2 = 1-mass(marg)
    p_marg = m1/(m1+m2)
    return p_marg

def original_model_reuse():
    A = flip(.4)
    if A:
        B = flip(1/3.0)
        return B
    else:
        B = flip(1/3.0)
        return not(B)

# no possibility of divergence in this model
def model_reuse_1():
    diverged = False
    if flip(.4*(1/3.0)):
        return True
    else:
        Amarg = { True : .4 }
        Aunexplored = { True : 2/3.0 }
        Amarg_prob = choose_marg_prob(Amarg, Aunexplored)
        if flip(Amarg_prob):
            A = sample_dist(multiply_dists(Amarg, Aunexplored))
        else:
            A = False
            diverged = True
        if A:
            Bmarg = { True : 1/3.0 }
            Bunexplored = { True : 0.0 }
            Bmarg_prob = 0.0
            B = False
            return B
        else:
            Bmarg = { True : 1/3.0 }
            Bunexplored = { True : 1.0 }
            Bmarg_prob = choose_marg_prob(Bmarg, Bunexplored)
            if flip(Bmarg_prob):
                B = sample_dist(Bmarg)
            else:
                B = False
            return not(B)

def model_reuse_2():
    diverged = False
    m1 = .4*(1/3.0)
    m2 = .6*(2/3.0)
    if flip(m1+m2):
        return True
    else:
        Amarg = { True : .4, False : .6 }
        Aunexplored = { True : 2/3.0, False: 1/3.0 }
        Amarg_prob = choose_marg_prob(Amarg, Aunexplored)
        if flip(Amarg_prob):
            A = sample_dist(multiply_dists(Amarg, Aunexplored))
        else:
            raise Exception
        Bmarg = { True : 1/3.0, False: 2/3.0 }
        if A:
            Bmarg_local = { True : 1/3.0 }
            Bmarg_diff = { False : 2/3.0 }
            B = sample_dist(Bmarg_diff) # FIXME: process unclear
            return B
        else:
            Bmarg_local = Bmarg
            Bunexplored = { True : 1.0, False : 0.0 }
            Bmarg_prob = choose_marg_prob(Bmarg, Bunexplored)
            if flip(Bmarg_prob):
                B = sample_dist(multiply_dists(Bmarg, Bunexplored))
            else:
                raise Exception
            return not(B)


def sample_A():
    if flip(.3):
        A = True
    else:
        if flip(.6/.7):
            A = True
        else:
            A = False
    return A

def combine(A1, A2, A3):
    if A1:
        if A2:
            if A3:
                return True
            else:
                return False
        else:
            if A3:
                return True
            else:
                return False
    else:
        if A2:
            if A3:
                return False
            else:
                return True
        else:
            if A3:
                return True
            else:
                return False

def original_model_subs():
    A1 = sample_A()
    A2 = sample_A()
    A3 = sample_A()
    return combine(A1, A2, A3)

# def model_subs_1():
#     diverged = False
#     if flip(.3*.6*.3):
#         return True
#     else:
#         A1_marg = { True : .3  }
#         A1_unexplored = { True : 1-.6*.3, False : 1.0 }
#         A1_marg_prob = choose_marg_prob(A1_marg, A1_unexplored)
#         if flip(A1_marg_prob):
#             A1 = sample_dist(multiply_dists(A1_marg, A1_unexplored))
#         else:
#             A1 = sample_dist({ True : .6/.7, False : .1/.7})
#             diverged = True
#         if A1:
#             ...
#         else:
#             assert diverged == True
#             A2 = sample_dist(A1)
#             A3 = sample_A()            
#             ...
        

print_dist(original_model_subs)
