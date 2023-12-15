import re

from typing import List

def apply_map_interval(num: int, map_interval) -> int:
    dest_start, src_start, _ = map_interval
    offset = num - src_start
    return dest_start + offset

def compute_map_dest(num: int, map_intervals: List) -> int:
    if not map_intervals:
        return num
    else:
        next_num = apply_map_interval(num, map_intervals[0])
        return compute_map_dest(next_num, map_intervals[1:])

def compute_dest(num, maps):
    if not maps:
        return num
    else:
        next_num = compute_map_dest(num, maps[0])
        return compute_dest(next_num, maps[1:])

def find_min_location(
        seed_ranges: List,
        maps: List,
        min_location: int = float("inf")
) -> int:
    """Return the minimum For all the seeds defined by the input seed ranges and the src-dest maps."""
    if not seed_ranges:
        # we've processed ALL the seeds defined by the seed ranges
        return min_location

    start = seed_ranges[0]
    end = start + seed_ranges[1]

    print(f'{start} - {end}')

    for seed in range(start, end):
        location = compute_dest(seed, maps)
        min_location = min(min_location, location)

    return find_min_location(seed_ranges[2:], maps, min_location)


# This recursive approach fails with sys.setrecursionlimit(10000000)...
# Because the seed ranges are too large
# def find_min_location(
#         seed_ranges: List,
#         maps: List,
#         min_location: int = float("inf")
# ) -> int:
#     """Return the minimum For all the seeds defined by the input seed ranges and the src-dest maps."""
#     if not seed_ranges:
#         # we've processed ALL the seeds defined by the seed ranges
#         return min_location
#     elif seed_ranges[0] == seed_ranges[1]:
#         # we've finished processing the current seed range so start next range
#         return find_min_location(seed_ranges[2:], maps, min_location)
#     else:
#         # Check current seed destination and continue processing rest of seeds
#         curr_seed = seed_ranges[0]
#         curr_seed_location = compute_dest(curr_seed, maps)
#         min_location = min(min_location, curr_seed_location)
#         # Update the curr seed range for recursive calls
#         seed_ranges[0] += 1
#         seed_ranges[1] -= 1
#         return find_min_location(seed_ranges, maps, min_location)

def parse_input_file(input_file: str) -> dict:
    """
    Returns: {seed_ranges: [int], maps: [[[int int int], ...] ...]}
    """
    seed_ranges = []
    maps = []

    def extract_nums(s):
        return [int(n) for n in re.findall(r'\d+', s)]

    with open(input_file, 'r') as f:
        while line := f.readline():
            if not seed_ranges:
                seed_ranges = extract_nums(line)
            elif line.endswith(":\n"):
                # Prepare processing of new map
                maps.append([])
            elif line.startswith("\n"):
                continue
            else:
                maps[-1].append(extract_nums(line))

    return {'seed_ranges': seed_ranges, 'maps': maps}


def main(input_file: str) -> int:
    input_data = parse_input_file(input_file)
    seed_ranges = input_data['seed_ranges']
    maps = input_data['maps']
    return find_min_location(seed_ranges, maps, float('inf'))

if __name__ == "__main__":
    input_file = "./input.txt"
    res = main(input_file)
    print(res)
