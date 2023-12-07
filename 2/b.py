# Advent of Code Day 2 Part 2
import re
from functools import reduce

def get_lines(input_file):
    with open(input_file, "r") as f:
        return [line.rstrip() for line in f]

# Without regex
def find_game_color_set_power(game):
    blue_max = 0
    green_max = 0
    red_max = 0

    # Transform game text data to list of lists organized by game set
    sets = game.split(": ")[1]
    sets = sets.split("; ")
    sets = [s.split(", ") for s in sets]

    # Find the highest value for each color in each set
    for x in sets:
        for sample in x:
            n, color = sample.split(" ")
            n = int(n)
            if color == "red":
                red_max = max(red_max, n)
            if color == "green":
                green_max = max(green_max, n)
            if color == "blue":
                blue_max = max(blue_max, n)

    return blue_max * red_max * green_max

# With regex
def find_game_color_set_power(game: str):
    colors = ["green", "red", "blue"]

    def find_color_max(color: str):
        res = 0
        for x in re.findall(f"\d+ {color}", game):
            n = int(x.split(" ")[0])
            res = max(res, n)
        return res

    return reduce(lambda a, b: a * b, map(find_color_max, colors), 1)

def main(input_file):
    games = get_lines(input_file)
    return sum(map(find_game_color_set_power, games))

if __name__ == "__main__":
    input_file = "/home/ajpkim/pg/advent-code-2023/2/input.txt"
    res = main(input_file)
    print(res)
