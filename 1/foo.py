DIGITS = {str(d): True for d in range(10)}

def word_to_num(word: str) -> int:
    nums = [int(ch) for ch in word if ch in DIGITS]
    return nums[0] * 10 + nums[-1]

def read_words(input_file):
    with open(input_file, 'r') as f:
        return f.readlines()

def main(input_file):
    return sum(map(word_to_num, read_words(input_file)))

if __name__ == "__main__":
    input_file =  "/home/ajpkim/pg/advent-code-2023/1/input.txt"
    res = main(input_file)
    print(res)
    # 55123
