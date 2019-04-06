"""GCJ 2019, Qualification: Problem 1 - Foregone Solution

By Matt Snider, 2019-04-06
"""

def solve(N):
    """At least one digit of N is a 4. Find two digits A, B whose sum is N
    but do *not* contain a 4 themselves. A, B are positive integers.

    To accomplish this, we can simply find each position Px in the number that
    is a 4, where P=0 is the rightmost digit, and substract that from N, such
    that:

    B = (10^P1 + 10^P2 + ...)
    A = N - B

    e.g.
    4: B = 10^0 = 1
       A = 4 - 1 = 3

    14: B = 10^0 = 1 
        A = 14 - 1 = 13

    44: B = 10^0 + 10^1 = 11
        A = 44 - 11 = 33

    148: B = 10^1 = 10
         A = 148 - 10 = 138

    444: B = 10^0 + 10^1 + 10^2 = 111
         A = 444 - 111 = 333
    """
    B = 0
    for (P, n) in enumerate(reversed(str(N))):
        if n == '4':
            B += 10**P
    A = N - B
    return (A, B)


if __name__ == '__main__':
    T = int(input())

    for i in range(T):
        N = int(input())
        A, B = solve(N)
        print('Case #{}: {} {}'.format(i + 1, A, B))

