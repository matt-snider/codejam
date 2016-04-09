"""Google Code Jam 2016

Qualification Round
Problem 1C: Coin Jam

By Matt Snider
2016-04-09
"""
from collections import defaultdict


MAX_SIMPLE_FACTOR = 10**6
jamcoin_factors = defaultdict(list)


def jamcoin_candidates(size):
    candidate = 2**(size - 1) + 1
    while candidate < 2**size:
        yield int(bin(candidate)[2:])      
        candidate += 2


def is_jamcoin(n):
    """Checks if the number n represents a jamcoin"""
    i = 10
    while i >= 2:
        at_base = int(str(n), base=i)
        if is_prime(at_base):
            return False
        i -= 1
    return True


def is_simple_jamcoin(n):
    """Checks if the number n represents a simple jamcoin.

    A simple jamcoin is a jamcoin that can be verified easily. 
    In particular, it has a factor below some number MAX_SIMPLE_FACTOR.
    """
    global jamcoin_factors

    i = 10
    while i >= 2:
        factor = get_first_factor_at_base_below(n, 
                                                base=i, 
                                                limit=MAX_SIMPLE_FACTOR)
        if factor != -1:
            # Save value to avoid heavy recalculation
            jamcoin_factors[n].insert(0, factor)
        else:
            return False
        i -= 1
    return True


def is_prime(n):
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return False
    return True

def find_jamcoins(size, n):
    """Finds n jamcoins of the given size."""
    jamcoins = []
    for candidate in jamcoin_candidates(size):
        if len(jamcoins) == n:
            break
        if is_simple_jamcoin(candidate):
            jamcoins.append(candidate)
    return jamcoins


def get_first_factor_at_base(n, base):
    """Get's the first non-trivial factor at a specific base"""
    n = int(str(n), base)
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return i
    raise ValueError('{} has no factors'.format(n))


def get_first_factor_at_base_below(n, base, limit):
    """Get's the first non-trivial factor at a specific base below a limit"""
    n = int(str(n), base)
    stop = min(int(n**0.5) + 1, limit)
    for i in range(2, stop):
        if n % i == 0:
            return i
    return -1


if __name__ == '__main__':
    _ = input()
    size, n = map(int, input().split())
    jamcoins = find_jamcoins(size, n)
    if len(jamcoins) != n:
        exit('Not enough jamcoins ' 
             'using heuristic MAX_SIMPLE_FACTOR={}'.format(MAX_SIMPLE_FACTOR))

    print('Case #1:')
    for jamcoin in jamcoins:
        factors = jamcoin_factors[jamcoin]
        print('{} {}'.format(jamcoin, ' '.join(map(str, factors))))

