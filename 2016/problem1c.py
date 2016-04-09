"""Google Code Jam 2016

Qualification Round
Problem 1C: Coin Jam

By Matt Snider
2016-04-09
"""


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
        if is_jamcoin(candidate):
            jamcoins.append(candidate)
    return jamcoins


def get_first_factor_at_base(n, base):
    n = int(str(n), base)
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return i
    raise ValueError('{} has no factors'.format(n))

if __name__ == '__main__':
    _ = input()
    size, n = map(int, input().split())
    jamcoins = find_jamcoins(size, n)
    print('Case #1:')
    for jamcoin in jamcoins:
        factors = [get_first_factor_at_base(jamcoin, b) for b in range(2, 11)]
        print('{} {}'.format(jamcoin, ' '.join(map(str, factors))))

