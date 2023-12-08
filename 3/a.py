import re
from typing import List

def file_lines(input_file: str) -> List:
    with open(input_file, "r") as f:
        return [line.rstrip() for line in f]

def is_symbol(ch):
    return not (ch.isalnum() or ch == ".")

def has_symbol_neighbor(grid, row, col) -> bool:
    """Check if a position in the grid has a symbol neighbor."""
    rows = len(grid)
    cols = len(grid[0])
    # Constraints on indices to avoid out of bounds lookups
    bottom_row = min(rows-1, row+1)
    top_row = max(0, row-1)
    right_col = min(cols-1, col+1)
    left_col = max(0, col-1)
    neighbors = [
        grid[top_row][left_col],
        grid[top_row][col],
        grid[top_row][right_col],
        grid[row][left_col],
        grid[row][right_col],
        grid[bottom_row][left_col],
        grid[bottom_row][col],
        grid[bottom_row][right_col],
    ]
    return any(map(is_symbol, neighbors))

def create_symbol_mask(grid):
    """Return a 2D array with 1s in positions that HAVE a symbol neighbor."""
    rows = len(grid)
    cols = len(grid[0])
    symbol_mask = [[0] * cols for _ in range(rows)]

    for row in range(rows):
        for col in range(cols):
            symbol_mask[row][col] = int(has_symbol_neighbor(grid, row, col))

    return symbol_mask

def main(input_file):
    grid = file_lines(input_file)
    symbol_mask = create_symbol_mask(grid)
    total = 0

    for row, line in enumerate(grid):
        matches = re.finditer(r'\d+', line)
        for match in matches:
            cols = range(match.start(), match.end())
            if any(map(lambda col: symbol_mask[row][col], cols)):
                total += int(match.group())

    return total

if __name__ == "__main__":
    input_file = "./input.txt"
    res = main(input_file)
    print(res)
# 550934
