from typing import List

# This solution is WRONG because it treats every index as an individual digit
# so a number like 543 is parsed here as 5, 4, and 3...

def file_lines(input_file: str) -> List:
    with open(input_file, "r") as f:
        return [line.rstrip() for line in f]

def pad_input_grid(grid):
    pad_row = "." * len(grid[0])
    padded_grid = [pad_row, *grid, pad_row]

    for i, e in enumerate(padded_grid):
        padded_grid[i] = "." + e + "."

    return padded_grid

def is_symbol(ch):
    return not (ch.isalnum() or ch == ".")

def has_symbol_neighbor(grid, row, col) -> bool:
    neighbors = [
        grid[row-1][col-1],
        grid[row-1][col],
        grid[row-1][col+1],
        grid[row][col-1],
        grid[row][col+1],
        grid[row+1][col-1],
        grid[row+1][col],
        grid[row+1][col+1],
    ]
    return any(map(is_symbol, neighbors))

def create_symbol_mask(padded_grid):
    rows = len(padded_grid) - 2
    cols = len(padded_grid[0]) - 2
    symbol_mask = [[0] * cols for _ in range(rows)]

    for row in range(rows):
        for col in range(cols):
            symbol_mask[row][col] = int(has_symbol_neighbor(padded_grid, row, col))

    return symbol_mask

def main(input_file):
    grid = file_lines(input_file)
    padded_grid = pad_input_grid(grid)
    symbol_mask = create_symbol_mask(padded_grid)
    total = 0

    for row in range(len(grid)):
        for col in range(len(grid[0])):
            if (symbol_mask[row][col] == 1) and (grid[row][col].isdigit()):
                total += int(grid[row][col])

    return total

if __name__ == "__main__":
    input_file = "./input.txt"
    res = main(input_file)
    print(res)
