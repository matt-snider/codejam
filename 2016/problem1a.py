"""Google Code Jam 2016

Qualification Round
Problem A: Counting Sheep

By Matt Snider
2016-04-09
"""


def get_distinct_digits(n):
    return set(str(n))


def will_sleep(n):
    # TODO: adapt for large numbers
    if n == 0:
        return 'INSOMNIA'
    i = 1
    seen = set()
    while True:
        curr_n = i * n
        seen |= get_distinct_digits(curr_n)
        if len(seen) == 10:
            return curr_n
        i += 1


if __name__ == '__main__':
    T = int(input())
    for i in range(t):
        n = input()
        result = will_sleep(n)
        print('Case #{}: {}'.format(i + 1, result))

