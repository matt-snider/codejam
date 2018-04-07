""" Google Code Jam 2018
    Go, Gopher!

    Prepare rectangular tree plots in the orchard by requesting
    that the Go Gopher dig. The Gopher doesn't do exactly what
    is requested, but we find out where it did dig.

    Strategy: prepare the orchard in 3x3 blocks beginning at
    the top right corner, moving across until the desired area
    has been reached.

    Always request that the Gopher prepare the center of the current
    3x3 block until that block is finished.
"""
import sys

PLOT_START = 2
DEBUG = False


# Data models
class Block:
    def __init__(self, center_x, center_y):
        self.x = center_x
        self.y = center_y
        self._count_done = 0
        self._cells = [
            [False, False, False],
            [False, False, False],
            [False, False, False]
        ]

    def mark_done(self, cell_x, cell_y):
        index_x = cell_x - (self.x - 1)
        index_y = cell_y - (self.y - 1)
        if (index_x < 0 or index_x > 2) or (index_y < 0 or index_y > 2):
            raise IndexError('{}: ({}, {})'.format(self, cell_x, cell_y))

        if not self._cells[index_x][index_y]:
            self._cells[index_x][index_y] = True
            self._count_done += 1

    def is_done(self):
        return self._count_done == 9

    def __str__(self):
        return 'Block[x={},y={}]'.format(self.x, self.y)


# Main algorithm
def dig_plot(size):
    block = next_block(size)
    deploy_gopher(block.x, block.y)

    for (x, y) in judge_responses():
        block.mark_done(x, y)

        if block.is_done():
            block = next_block(size, block)
            debug('Block complete. Next block: {}'.format(block))

        deploy_gopher(block.x, block.y)


def next_block(plot_size, curr_block=None):
    if not curr_block:
        return Block(PLOT_START + 1, PLOT_START + 1)

    # Get the block to the right of and adjancent to
    # the current one.
    return Block(curr_block.x + 3, curr_block.y)


def deploy_gopher(x, y):
    print('{} {}'.format(x, y))


def judge_responses():
    while True:
        x, y = (int(x) for x in input().split())
        if x == -1 and y == -1:
            debug('Judge: error')
            raise Exception('Judge: invalid data sent or 1000 tries reached')
        elif x == 0 and y == 0:
            debug('Judge: correct')
            raise StopIteration()

        debug('Judge: {} {}'.format(x, y))
        yield (x, y)


def debug(msg):
    if DEBUG:
        print(msg, file=sys.stderr)


if __name__ == '__main__':
    T = int(input())
    for i in range(T):
        size = int(input())
        dig_plot(size)
