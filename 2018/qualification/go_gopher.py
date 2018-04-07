""" Google Code Jam 2018
    Go, Gopher!

    Prepare rectangular tree plots in the orchard by requesting
    that the Go Gopher dig. The Gopher doesn't do exactly what
    is requested, but we find out where it did dig.

    Strategy: prepare the orchard in 3x3 blocks beginning at
    the top right corner, moving across, and then on to the
    next row. Do not leave a 3x3 block until it has been fully
    prepared.

    Request that the Gopher prepare the center space every single
    time until the block is complete.
"""
import sys

PLOT_START = 2


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
            raise IndexError(f'{self}: ({cell_x}, {cell_y})')

        if not self._cells[index_x][index_y]:
            self._cells[index_x][index_y] = True
            self._count_done += 1
        debug(f'mark_done({index_x}, {index_y}) -> done {self._count_done}')

    def mark_rows_done(self, n_rows):
        if n_rows > 3:
            raise IndexError(f'{self} does not have {n_rows} rows')
        for i in range(0, n_rows):
            self._cells[i] = [True, True, True]

    def mark_cols_done(self, n_cols):
        if n_cols > 3:
            raise IndexError(f'{self} does not have {n_cols} columns')

        for row in self._cells:
            for i in range(0, n_cols):
                self._cells[row][i] = True

    def is_done(self):
        return self._count_done == 9

    def __str__(self):
        return f'Block[x={self.x},y={self.y}]'


# Main algorithm
def dig_plot(size):
    block = next_block(size)
    deploy_gopher(block.x, block.y)

    for (x, y) in judge_responses():
        block.mark_done(x, y)

        if block.is_done():
            block = next_block(size, block)
            debug(f'Block complete. Next block: {block}')

        deploy_gopher(block.x, block.y)


def next_block(plot_size, curr_block=None):
    if not curr_block:
        return Block(PLOT_START + 1, PLOT_START + 1)

    # Get the block to the right of and adjancent to
    # the current one. If this block spills over the
    # plot's edge, move back and clear any rows that
    # have already been prepared.
    beside_x = curr_block.x + 3
    spill_amount = (beside_x + 1) - plot_size - PLOT_START
    if spill_amount <= 0:
        return Block(beside_x, curr_block.y)
    elif spill_amount < 3:
        block = Block(beside_x - spill_amount, curr_block.y)
        block.mark_cols_done(spill_amount)
        return block

    # Otherwise the entire row is finished. Return the first
    # block in the next row.
    below_y = curr_block.y + 3
    spill_amount = (below_y + 1) - plot_size - PLOT_START
    if spill_amount <= 0:
        return Block(PLOT_START, below_y)
    elif spill_amount < 3:
        block = Block(PLOT_START, below_y - spill_amount)
        block.mark_rows_done(spill_amount)
        return block

    # This would indicate we are done and shouldn't happen
    # since the judge will respond with 0 0 and end the game.
    raise ValueError('next_block() requested on finished plot')


def deploy_gopher(x, y):
    print(f'{x} {y}')


def judge_responses():
    while True:
        x, y = (int(x) for x in input().split())
        if x == -1 and y == -1:
            debug('Judge: error')
            raise Exception('Judge: invalid data sent or 1000 tries reached')
        elif x == 0 and y == 0:
            debug('Judge: correct')
            raise StopIteration()

        debug(f'Judge: {x} {y}')
        yield (x, y)


def debug(msg):
    print(msg, file=sys.stderr)


if __name__ == '__main__':
    T = int(input())
    for i in range(T):
        size = int(input())
        dig_plot(size)
