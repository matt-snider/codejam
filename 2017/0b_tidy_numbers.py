from collections import deque


def tidy_split(s):
    """Splits the input into a tidy and untidy section"""
    tidy = []
    untidy = deque(s)

    # Skip tidy section
    while untidy:
        digit = untidy[0]
        next_digit = untidy[1] if len(untidy) >= 2 else None
        if next_digit is None or int(digit) <= int(next_digit):
            tidy.append(digit)
            untidy.popleft()
        else:
            break
    return [''.join(tidy), ''.join(untidy)]


def tidy(n):
    tidy_section, untidy_section = tidy_split(n)
    if not untidy_section:
        return tidy_section
    else:
        # Tidy up the untidy section
        # Note: it is necessarily of len >= 2
        first = int(untidy_section[0]) - 1
        tidied = str(first) + '9' * (len(untidy_section) - 1)
        return tidy(tidy_section + tidied)


if __name__ == '__main__':
    T = int(input())
    for i in range(T):
        n = input()
        t = tidy(n).lstrip('0')
        print('Case #{}: {}'.format(i + 1, t))
