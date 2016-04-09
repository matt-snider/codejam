"""Google Code Jam 2016

Qualification Round
Problem 1B: Revenge of the Pancakes

By Matt Snider
2016-04-09
"""
HAPPY = '+'
BLANK = '-' 

def flip(pancakes, n=1):
    assert n >= 1
    if len(pancakes) == 1:
        return HAPPY if pancakes == BLANK else BLANK
    else:
        to_flip = pancakes[:n]
        return ''.join([flip(p) for p in to_flip]) + pancakes[n:]


def get_contiguous_len(s):
    """Get the number of pancakes in a row with the same side up"""
    top = s[0]
    try:
        return s.index(flip(top))
    except ValueError:
        return len(s)


def prepare_pancakes(pancakes):
    """Assure all pancakes are happy side up"""
    maneuvers = 0
    while BLANK in pancakes:
        maneuvers += 1
        
        # Do the maneuver
        i = get_contiguous_len(pancakes)
        pancakes = flip(pancakes, n=i)
    return maneuvers

