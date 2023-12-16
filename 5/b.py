import multiprocessing
import re
import datetime

from typing import List

def apply_map_interval(num: int, map_interval) -> int:
    """Find the output number given a starting number and map transformation."""
    dest_start, src_start, _ = map_interval
    offset = num - src_start
    return dest_start + offset

def compute_dest(num, maps):
    """Find the final location of a seed."""
    for m in maps:
        for map_interval in m:
            num = apply_map_interval(num, map_interval)
    return num

def process_chunk(chunk_args):
    """Parallel worker function for finding the min location in a chunk of a seed range."""
    start, end, maps = chunk_args
    min_location = float('inf')
    for seed in range(start, end):
        location = compute_dest(seed, maps)
        min_location = min(min_location, location)
    return min_location

def process_seed_range(start, end, maps, num_processes):
    """Process the given seed range in parallel."""
    # Prepare a sub-range for each CPU
    chunk_size = (end - start) // num_processes
    chunk_indices = []
    for i in range(start, end, chunk_size):
        chunk_indices.append([i, i + chunk_size])
    # make sure we cover the full seed range
    chunk_indices[-1][1] = end
    chunk_args = [(start, end, maps) for (start, end) in chunk_indices]

    with multiprocessing.Pool(num_processes) as pool:
        results = pool.map(process_chunk, chunk_args)

    return min(results)

def find_min_location(
        seed_ranges: List,
        maps: List,
        min_location: float = float('inf'),
        num_processes: int = 4
) -> int:
    if not seed_ranges:
        return min_location
    start, end = seed_ranges[0]

    print(f'New seed range with {end-start} seeds ..... {datetime.datetime.now().strftime("(%H:%M:%S)")}')

    min_location = process_seed_range(start, end, maps, num_processes)
    return find_min_location(seed_ranges[1:], maps, min_location, num_processes)


def parse_input_file(input_file: str) -> dict:
    """
    Returns: {seed_ranges: [(start, end), ...], maps: [[[int int int], ...] ...]}
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

    # Organize the seed ranges nicely as (start, end) ranges
    seed_ranges = [(seed_ranges[i], seed_ranges[i] + seed_ranges[i+1])
                   for i in range(len(seed_ranges)-1)]

    return {'seed_ranges': seed_ranges, 'maps': maps}


def main(input_file: str, num_processes: int = 4) -> int:
    input_data = parse_input_file(input_file)
    seed_ranges = input_data['seed_ranges']
    maps = input_data['maps']
    return find_min_location(seed_ranges, maps, float('inf'), num_processes)

if __name__ == "__main__":
    input_file = "./input.txt"
    num_processes = multiprocessing.cpu_count() - 4
    res = main(input_file, num_processes)
    print(res)
